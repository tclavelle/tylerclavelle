library(tidyverse)
library(httr)
library(stringr)
library(lubridate)

## Search for artists by name
get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', ''), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA)
        )
    })
    
    return(artists)
}

## Get artist albums
get_albums <- function(artist_uri, lim = 50, off = 0) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums?album_type=album&limit=',lim,'&offset=',off)) %>% content
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                       album_name = str_replace_all(tmp$name, '\'', ''),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', ''))) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_year)
}

## Get tracks
get_tracks <- function(artist_info, album_info, id, secret) {
    
    client_id <- id
    client_secret <- secret
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) %>% 
            select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) %>%
        mutate(artist_img = artist_info$artist_img) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    return(track_info)
}

# ### Phish.net helper functions
# API and user keys
api_key <- 'B53003FE66383F8D79E7'
app_key <- '0A3D9E14542B42F37A5E'

# API endpoint
end_point <- 'https://api.phish.net/v3/'

# Get table of all jamcharts
# jc_all <- GET(paste0(end_point,'jamcharts/all?apikey=',api_key)) %>% content
# jc_all <- jc_all$response[2]$data

# Convert response to data frame
# jc_all <- map_df(seq_len(length(jc_all)), function(x) {
#   list(song = jc_all[[x]]$song,
#        songid = jc_all[[x]]$songid,
#        jams   = jc_all[[x]]$items
#   )
# })

# top_jams <- c('You Enjoy Myself', 'Tweezer', 'Bathtub Gin', 'Chalk Dust Torture', 'David Bowie',
#               "Down with Disease", 'Runaway Jim', 'Twist', 'Piper', 'Ghost', 'Also Sprach Zarathustra',
#               'Light')
# 
# # Get info for top songs
# top_jam_ids <- filter(jc_all, song %in% top_jams)

# Function to get individual jamcharts
jam_chart <- function(songid, apikey = api_key) {
  
  jc <- POST(paste0(end_point,'jamcharts/get?songid=',songid,'&apikey=',api_key)) %>% content %>% .$response %>% .$data %>% .$entries
  
  # Clean response and combine all returned artists into a dataframe
  jc2 <- map_df(seq_len(length(jc)), function(x) {
    list(songid = songid,
         date = jc[[x]]$showdate,
         highly_rec = jc[[x]]$marked_recommended)
  }) %>% bind_rows()

  return(jc2)
}

# # Pull jamcharts for top_jam_ids
# top_jams_charts <- lapply(top_jam_ids$songid, FUN = jam_chart) %>%
#   bind_rows() %>%
#   left_join(top_jam_ids)

# test <- top_songs %>%
#   filter(song %in% top_jams) %>%
#   left_join(top_jams_charts)

client_id <- '8ec1c22eb38d4e0b9e10e8b715a5f74e'
client_secret <- '5636a032ece24f38aad8cbd01b266ec2'

## Get tracks
get_playlist <- function(user_id, playlist_id, id, secret, off) {

  # client_id <- id
  # client_secret <- secret
  access_token <- POST('https://accounts.spotify.com/api/token',
                       accept_json(), authenticate(id, secret),
                       body = list(grant_type='client_credentials'),
                       encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
  tracks <- GET(paste0('https://api.spotify.com/v1/users/', user_id,'/playlists/', playlist_id,'/tracks?offset=',off),
                query = list(access_token = access_token)) %>% content %>% .$items 
  
  tracks <- map_df(seq_len(length(tracks)), function(x) {
    list(
      track_name       = tracks[[x]]$track$name, 
      album_name       = tracks[[x]]$track$album$name,
      track_popularity = tracks[[x]]$track$popularity,
      track_href       = tracks[[x]]$track$href,
      track_uri        = tracks[[x]]$track$uri
      )
  }) 
  
  return(tracks)
}

# Find Phish's artist id info
# phish <- get_artists('Phish') %>%
#   filter(artist_name == 'Phish')
# 
# # Get all albums. Need to make several calls because there are more than 50 albums
# phish_albums <- get_albums(artist_uri = phish$artist_uri) %>%
#   bind_rows(get_albums(artist_uri = phish$artist_uri, off = 50))
# 
# # Studio albums to ignore
# studio <- c('The Siket Disc','The Story Of The Ghost', 'Farmhouse', 'Round Room', 'Joy', 'Fuego', 'Big Boat',
#             'Undermind', 'Junta', '(Hoist)', 'Billy Breathes', 'Rift', 'A Picture Of Nectar', 'Lawn Boy',
#             'The White Tape')
# 
# # Filter to only live albums
# livephish <- filter(phish_albums, !album_name %in% studio)
# 
# # Get track info
# live_tracks <- get_tracks(artist_info = phish, id = client_id, secret = client_secret, album_info = livephish)
# 
# # Clean track names
# live_tracks$track_name <- gsub(pattern = ' - Live', replacement = '', live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " [Live At Madison Square Garden, New Year's Eve 1995]", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " [Live In Brooklyn]", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " - Hampton, 1998", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " - Clifford Ball, 1994", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " [Live At Madison Square Garden]", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " [Live At Madison Square Garden]", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " [Live Version - Clifford Ball, 1994]", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " 1", replacement = '', fixed = TRUE, live_tracks$track_name)
# live_tracks$track_name <- gsub(pattern = " 2", replacement = '', fixed = TRUE, live_tracks$track_name)                            
# # Save data for offline anaysis
# # write_csv(live_tracks, path = 'data/spotify_phish_tracks.csv')

# spotify:user:1265043326:playlist:3FdZNyZ42vMB5szof9EA3N
# test <- get_playlist(user_id = '1265043326', playlist_id = '3FdZNyZ42vMB5szof9EA3N',
#                      id = client_id, secret = client_secret)
  # spotify_df <- get_tracks(artist_info, album_info)

# str(spotify_df)

