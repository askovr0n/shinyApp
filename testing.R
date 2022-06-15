install.packages("spotifyr")
install.packages("kable")
library(tidyverse)
library(spotifyr)
library(lubridate)
library(kable)
library(stats)
library(ggpubr)
library(factoextra)

Sys.setenv(SPOTIFY_CLIENT_ID = '65bd18c74fd34be0a24117ab05b98b40')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '34bd47c92edf465993287ff4b707bf38')

access_token <- get_spotify_access_token()


#> Warning: package 'lubridate' was built under R version 4.1.1

get_my_recently_played(limit = 5) %>% 
  mutate(
    artist.name = map_chr(track.artists, function(x) x$name[1]),
    played_at = as_datetime(played_at)
  ) %>% 
  select(
    all_of(c("track.name", "artist.name", "track.album.name", "played_at"))
  )

11142493166
l72qiwa7koqcqmlxkdertq40q

my_id <- '11142493166'
my_plists <- get_user_playlists(my_id)

my_plists2 <- my_plists %>% 
  filter(name %in% c('You & I Radio', 'Dance Party')) %>% 
  select(id)

tracks <- my_plists2[['id']] %>% lapply(get_playlist_tracks)
features <- tracks %>% select(track.id) %>% lapply(get_track_audio_features) %>% tibble()

tracks2 <- tracks %>%
  left_join(features[[1]][[1]], by = c("track.uri" = "uri"))

tracks2 %>% 
  select(danceability:tempo) %>% as.matrix() %>% 
  kmeans(3) -> kmeans2

tracks2 %>% 
  select(danceability:tempo) %>%
  fviz_cluster(kmeans2, .)
