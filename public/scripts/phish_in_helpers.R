library(tidyverse)
library(httr)
library(stringr)
library(wordcloud2)

# Get data from phish.in API for song info
# song_res <- GET('http://phish.in/api/v1/songs.json?per_page=10000')
# song_raw <- song_res %>% content %>% .$data
# song_counts <- map_df((seq_along(song_raw)), function(x) {
#   list(song         = song_raw[[x]]$title,
#        song_id      = song_raw[[x]]$id,
#        performances = song_raw[[x]]$tracks_count)
# })

# # Get data from phish.in API for tracks
track_info <- function(track_id) {
res <- GET(paste0('http://phish.in/api/v1/tracks/',track_id,'.json?per_page=10000'))
raw <- res %>% content %>% .$data

# Convert to data_frame
track_df <- map_df(seq_along(raw), function(x) {
  list(song       = raw$title,
       song_id    = raw$song_ids[[1]],
       track_id   = raw$id,
       set        = raw$set_name,
       date       = raw$show_date,
       position   = raw$position,
       length     = raw$duration / 1000 / 60,
       mp3        = raw$mp3,
       slug       = raw$slug)
})
return(track_df)
}
  
# Write function to get data for all performances of a desired song
song_info <- function(song_id) {
  track <- GET(paste0('http://phish.in/api/v1/songs/',song_id,'.json?per_page=1000'))
  raw <- track %>% content %>% .$data %>% .$tracks 
  
  # Convert to data_frame
  song_df <- map_df(seq_along(raw), function(x) {
    list(song       = raw[[x]]$title,
         track_id   = raw[[x]]$id,
         set        = raw[[x]]$set,
         date       = raw[[x]]$show_date,
         position   = raw[[x]]$position,
         length     = raw[[x]]$duration / 1000 / 60,
         mp3        = raw[[x]]$mp3,
         slug       = raw[[x]]$slug)
  })
  return(song_df)
}

# Write function to get data for locations of all performances
venue_info <- function(venue_id) {
  # venue <- GET(paste0('http://phish.in/api/v1/venues/',venue_id,'.json?per_page=500'))
  venues <- GET(paste0('http://phish.in/api/v1/venues.json?per_page=1000'))
  raw <- venues %>% content %>% .$data
  
  # Convert to data_frame
  venue_df <- map_df(seq_len(length(raw)), function(x) {
    temp_venue <- list(venue_name = raw[[x]]$name,
         location   = raw[[x]]$location,
         shows      = raw[[x]]$shows_count)
    
  }) %>%
    separate(location, into = c('city', 'state'), sep = ', ')
  
  return(venue_df)
}

venue_ids <- GET(paste0('http://phish.in/api/v1/venues.json?per_page=1000'))

venue_ids %>% content %>% .$data

# # pull out all performances for every song in top jams
# top_songs <- lapply(song_counts$song_id[song_counts$song %in% top_jams], song_info) %>%
#   bind_rows()
# 
# # song summary (use to determine which jams to explore)
# top_song_summary <- top_songs %>%
#   group_by(song) %>%
#   summarize(count = length(song),
#             avg_length = mean(length, na.rm = T),
#             sd_length  = sd(length, na.rm = T)) %>%
#   filter(grepl('>', song) == F & is.na(sd_length)==F) %>%
#   mutate(type_2 = avg_length + sd_length)
# 
# # save data for offline work
# # write_csv(top_songs, path = '../My-code/phishfromvt/www/phish_in.csv')
# 
# # plot distributions
# top_songs %>%
#   filter(song %in% top_jams) %>%
#   left_join(top_song_summary) %>%
#   mutate(is_type_2 = length >= type_2) %>%
#   ggplot(aes(x = length, fill = is_type_2)) +
#   geom_histogram(binwidth = 1) +
#   geom_vline(data = filter(top_song_summary, song %in% top_jams),
#              aes(xintercept = avg_length),
#              linetype = 2) +
#   scale_fill_brewer(palette = 'Set1') +
#   facet_wrap(~song, scales = 'free_y')
# 
# # Average length per year lineplot
# top_songs %>%
#   filter(song %in% top_jams) %>%
#   group_by(song, date) %>%
#   summarize(length = sum(length, na.rm = T)) %>%
#   separate(date, into = c('year', 'month', 'day'), sep = '-') %>%
#   mutate(year = as.numeric(year)) %>%
#   group_by(song, year) %>%
#   summarize(avg_length = mean(length, na.rm = T),
#             count      = length(song)) %>%
#   complete(year=full_seq(year, period = 1), nesting(song), fill = list(0)) %>%
#   ungroup() %>%
#   ggplot(aes(x = year, y = avg_length, group = song)) +
#   geom_rect(aes(xmin = min(year, na.rm = T), xmax = 2000, 
#                 ymin = 0, ymax = max(avg_length, na.rm = T)),
#             fill = 'lightgreen', alpha = 0.2) +
#   geom_rect(aes(xmin = 2002, xmax = 2004, 
#                 ymin = 0, ymax = max(avg_length, na.rm = T)),
#             fill = 'orange', alpha = 0.2) +
#   geom_rect(aes(xmin = 2009, xmax = 2017, 
#                 ymin = 0, ymax = max(avg_length, na.rm = T)),
#             fill = 'lightblue', alpha = 0.2) +
#   geom_line() +
#   geom_point(aes(size = count)) +
#   scale_size_continuous(breaks = c(seq(10,60,by = 10))) +
#   facet_wrap(~song) +
#   theme_bw()

# # Pull out Mike's Groove examples and make word plot of the groove songs
# ms_groove <- lapply(song_counts$song_id[grepl("Mike's", song_counts$song) | 
#                                           grepl('Weekapaug', song_counts$song)], song_info) %>%
#   bind_rows() %>%
#   arrange(date, position) %>% # arrange by date and track_id to find the in between songs
#   group_by(date) %>% 
#   complete(track_id = full_seq(track_id, period = 1)) %>% # expand the track ids to add in the ids of the tracks in between
#   ungroup()
#   
# # find the songs in the groove
# ms_groove_2 <- lapply(ms_groove$track_id[is.na(ms_groove$song)], track_info) %>%
#   bind_rows()
# 
# # Pull out groove
# ms_groove_3 <- ms_groove %>%
#   filter(is.na(song)==F) %>%
#   bind_rows(ms_groove_2) %>%
#   arrange(date, track_id) %>%
#   distinct() %>%
#   filter(!song %in% c("Mike's Song", "Weekapaug Groove"))
# 
# # Fix songs with mike's and paug in the song title  
# ms_groove_3$song <- gsub(" > Weekapaug Groove", replacement = '', ms_groove_3$song)
# ms_groove_3$song <- gsub("Mike's Song >", replacement = '', ms_groove_3$song)
# ms_groove_3$song <- gsub(" I Am Hydrogen", replacement = 'I Am Hydrogen', ms_groove_3$song)
# 
# ms_groove_wc <- ms_groove_3 %>%
#   group_by(song) %>%
#   summarize(freq = length(song)) %>%
#   ungroup() %>%
#   rename(word = song) 

# figPath <- system.file("examples/cactus.png",package = "wordcloud2")
# wordcloud2(subset(ms_groove_wc, freq > 1), color = rep(c('green','darkgreen'),
#                                      length.out = nrow(ms_groove_wc)),
#            size = 0.5)

# Phish.net jam charts

