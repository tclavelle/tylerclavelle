#######################################################################################
## Project: Blog
## Purpose: Opiate epidemic analysis
## Date: 02/10/2018
#######################################################################################

### Packages
library(tidyverse)
library(stringr)
library(forcats)
library(viridis)
library(scales)
library(RColorBrewer) 
library(plotly)
library(ggplot2)
library(gganimate)
library(ggraph)
library(igraph)
library(tweenr)

# Read in opiate data sets
opioids <- read_csv(file = 'static/data/opioids.csv') # drug types
docs <- read_csv(file = 'static/data/prescriber-info.csv') # prescribers

# Overdose deaths
od_lg <- read_tsv('static/data/Underlying Cause of Death, 1999-2016_2.txt') # overall by state
od_gender <- read_tsv('static/data/ods_cdc_wonder_gender_race.txt') # by gender and race
od_age <- read_tsv('static/data/ods_cdc_wonder_age') # by gender age

# Convert prescriber info into long format
docs <- docs %>%
  select(NPI, Gender, State, Credentials, Specialty, Opioid.Prescriber, everything()) %>%
  gather(key = 'drug', value = 'prescriptions', 7:ncol(.)) %>%
  rename(npi = NPI, 
         gender = Gender, 
         state = State, 
         credentials = Credentials, 
         specialty = Specialty, 
         prescriber = Opioid.Prescriber)

# Process 1990-2016 OD data
od_lg <- od_lg %>% 
  select(State, Year, Deaths, Population, `Crude Rate`) %>% 
  mutate(deaths_per_100k = as.numeric(`Crude Rate`))

# Get data for state map
states <- map_data(map = 'state')

# Join overdose data with state map data
state_overdoses <- states %>%
  left_join(od_lg %>%
              rename(region = State) %>%
              mutate(region = tolower(region)))

od_gif <- state_overdoses %>%
  filter(is.na(Year)==F & Year > 2009) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = deaths_per_100k, frame = Year)) +
  geom_polygon() +
  scale_fill_viridis(option = 'A', direction = -1) +
  labs(title = 'Overdose deaths per 100,000 people',
       fill  = 'Deaths') +
  theme_bw()

 od_gif <- gganimate(od_gif)

 gganimate_save(od_gif, 
                filename = 'static/img/portfolio/opiate_map.gif', 
                saver = 'gif',
                width = 600,
                height = 400)

# Summarize deaths by gender
gender_gif_df <- od_gender %>% 
  filter(is.na(Gender)==F) %>% 
  group_by(Year, Gender) %>% 
  summarize(Deaths = sum(Deaths, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ease = "linear",
         x = Year) %>%
  rename(y  = Deaths,
         id = Gender,
         time = Year)

# Tweenr dataframe
gender_tween <- gender_gif_df %>% 
  tween_elements(., "time", "id", "ease", nframes = 100) %>%   #using tweenr!
  mutate(year = round(time), id = .group)

# Create ggplot from tweenr data set
gender_gif <- gender_tween %>% 
  ggplot(aes(x = x, y = y, color = .group, cumulative = TRUE, frame = .frame)) +
  geom_line() +
  guides(color = FALSE) +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(palette = 'Dark2') +
  annotate(geom = "text", x = 2003, y = 20000, label = "Men", 
           cex = 6, angle = 22, fontface = "bold", color = "#D95F02") +
  annotate(geom = "text", x = 2003, y = 12000, label = "Women", 
           cex = 6, angle = 22, fontface = "bold", color = "#1B9E77") +
  labs(y = "Deaths",
       x = "Year") +
  theme_minimal()

# render image
gender_gif_save <- gganimate(gender_gif, interval = 0.05, title_frame = F)

gganimate_save(gender_gif_save, filename = 'static/img/portfolio/opiate_tweenr.gif', 
               saver = 'gif', interval = 0.05)


# OD Circle Packing Graph ----------------------------------------------------------------

# Graph will have the following hierarchy
# + All ODs
#   + Gender ODs
#     + Age ODs
#       + Race ODs

edges <- data_frame(from = 'All', to = unique(od_age$Race))

edges <- edges %>% 
  group_by(to) %>% 
  mutate(to2 = list(map2_chr(to, unique(od_age$`Five-Year Age Groups Code`), ~paste(.x, .y, sep = '_')))) %>% 
  unnest() %>% 
  group_by(to2) %>% 
  mutate(to3 = list(map2_chr(to2, unique(od_age$Gender), ~paste(.x, .y, sep = '_')))) %>% 
  unnest() %>% 
  filter(!grepl('NA', to2) & !grepl('NA', to3))

# Build edge dataframe using series of expand grid calls with unique edge values
edges_df <- expand.grid(from = 'All', to = unique(edges$to)) %>% 
  bind_rows(edges %>% 
              select(to, to2) %>% 
              rename(from = to,
                     to   = to2)) %>%
  bind_rows(edges %>% 
              select(to2, to3) %>% 
              rename(from = to2,
                     to   = to3)) %>% 
  distinct()

# Build OD dataset by grouping by most complex group first and then mutating less specific values
edge_ods <- od_age %>% 
  filter(Year == max(Year, na.rm = T)) %>% 
  group_by(Gender, `Five-Year Age Groups Code`, Race) %>% 
  summarize(all_ods = sum(Deaths, na.rm = T)) %>% 
  mutate(to = paste(Race, `Five-Year Age Groups Code`, Gender, sep = '_')) %>% 
  ungroup() %>% 
  select(to, all_ods) %>% 
  bind_rows(od_age %>% 
              filter(Year == max(Year, na.rm = T)) %>% 
              group_by(Race, `Five-Year Age Groups Code`) %>% 
              summarize(all_ods = sum(Deaths, na.rm = T)) %>% 
              mutate(to = paste(Race, `Five-Year Age Groups Code`, sep = '_')) %>%
              ungroup() %>% 
              select(to, all_ods)) %>%
  bind_rows(od_age %>% 
              filter(Year == max(Year, na.rm = T)) %>% 
              group_by(Race) %>% 
              summarize(all_ods = sum(Deaths, na.rm = T)) %>% 
              rename(to = Race) %>% 
              ungroup() %>% 
  bind_rows(od_age %>% 
              filter(Year == max(Year, na.rm = T)) %>% 
              summarize(all_ods = sum(Deaths, na.rm = T)) %>% 
              mutate(to = 'All')))

# Left join edge OD data to edges
edges_graph_df <- edges_df %>% 
  tbl_df() %>% 
  left_join(edge_ods) %>% 
  filter(!is.na(all_ods))

# Make network graph data
od_graph_edges <- graph_from_data_frame(edges_graph_df)

ggraph(od_graph_edges, layout = 'circlepack') +
  geom_node_circle(aes(fill = depth)) +
  scale_fill_viridis() +
  guides(fill = F) +
  theme_minimal()

