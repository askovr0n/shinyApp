install.packages("spotifyr")
install.packages("kable")
library(tidyverse)
library(spotifyr)
library(lubridate)
library(kable)
library(stats)
library(ggpubr)
library(factoextra)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxx')

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

my_id <- '1166825425'
my_plists <- get_user_playlists(my_id)

my_plists2 <- my_plists %>% 
  filter(name %in% c('You & I Radio')) %>% 
  select(id)

tracks <- my_plists2[['id']] %>% lapply(get_playlist_tracks)
features <-
  tracks[[1]] %>% select(track.id) %>% lapply(get_track_audio_features) %>% tibble()

tracks2 <- tracks[[1]] %>%
  left_join(features[[1]][[1]], by = c("track.uri" = "uri"))

tracks2 %>% 
  select(danceability:tempo) %>% as.matrix() %>% 
  kmeans(3) -> kmeans2

tracks2 %>% 
  select(danceability:tempo) %>%
  fviz_cluster(kmeans2, .)

ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  select(added_at,track.artists,track.name,track.id) %>% 
  unnest() %>% 
  select(added_at, name, track.id, track.name) %>% 
  nest(name) -> y
 
  y %>% mutate(artistsnames = map(.x = y$data, .f = ~paste(.x$name, collapse = ", "))) %>% unnest() %>% select(-name) %>% distinct() -> y_new
  
  
 features <- y %>% 
   select(track.id) %>% 
   lapply(get_track_audio_features) %>% 
   as.data.frame() %>% 
   select(track.id.danceability:track.id.tempo, track.id.id, track.id.duration_ms) %>% 
   rename_all(~str_replace(.,"^track.id.",""))
 
 dataset <- y_new %>% left_join(features, by = c("track.id" = "id"))
   
  # reduce(rbind) %>% 
  # distinct()


all_my_fav_tracks <-
  # This is somehow tough to read, but I lOVE PIPELINE!
  # FIRST we send get_my_saved_tracks request, set include_meta_info to TRUE, will return the number of all tracks in total. After that, we request 50 items per time, therefore, the looptime should be 50 divide the length of tracks.
  # Not all of us is lucky man, so if the number of tracks can't be divided exactly, we make one more time offset.
  ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  # Generate a sequence by looptimes.
  seq() %>%
  # PlZ remember, the offset is start from zero, so we minus 1 from the offset. And everytime, we jump 50 items, because we have already request 50 items every time.
  # Every loop, we get a data.frame with 50 rows, once the loop finished, we got a list with looptime piece data.frame, with reduce(rbind), we can merge all the data.frames as one.
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind)


artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  # I don't think we need Urls in further analyses, id (unique mark of artist) and name are selected here.
  select(id, name)

track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n) %>% 
  arrange(desc(n))


ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind) %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name) %>%
  count(id, sort = TRUE) %>%
  left_join(ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
              seq() %>%
              map(function(x) {
                get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
              }) %>% reduce(rbind) %>%
              select(track.artists) %>%
              reduce(rbind) %>%
              reduce(rbind) %>%
              select(id, name), by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(2, n)  %>% 
  rename('Artist_Name' = 'name', 'Quantity' = n) %>% 
  arrange(desc(Quantity)) %>% mutate(Position = row_number()) %>% tibble() %>% unname() %>% as.matrix() %>% t()
