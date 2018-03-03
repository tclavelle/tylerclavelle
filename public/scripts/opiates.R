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
library(tweenr)

# Read in opiate data sets
opioids <- read_csv(file = 'content/portfolio/data/opioids.csv') # drug types
docs <- read_csv(file = 'content/portfolio/data/prescriber-info.csv') # prescribers

# Overdose deaths
od_lg <- read_tsv('content/portfolio/data/Underlying Cause of Death, 1999-2016_2.txt') # overall by state
od_gender <- read_tsv('content/portfolio/data/ods_cdc_wonder_gender_race.txt') # by gender and race
od_age <- read_tsv('content/portfolio/data/ods_cdc_wonder_age') # by gender age

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
  scale_fill_viridis(option = 'A') +
  labs(title = 'Overdose deaths per 100,000 people',
       fill  = 'Deaths') +
  theme_bw()

 od_gif <- gganimate(od_gif)

 gganimate_save(od_gif, filename = 'content/portfolio/img/opiate_map.gif', saver = 'gif')

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

gganimate_save(gender_gif_save, filename = 'content/portfolio/img/opiate_tweenr.gif', 
               saver = 'gif', fps = 5)