
authenticate <- function(id, secret) {
    # authenticate the spotify client stuff
    Sys.setenv(SPOTIFY_CLIENT_ID = id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
    
    access_token <- get_spotify_access_token()
}



server <- function(input, output) {
    validate <- observeEvent(input$btn, {authenticate(input$id, input$secret)})
    output$validate_message <- renderText({validate() }) 
    
    top_5_artists <- reactive({get_my_top_artists_or_tracks(type = 'artists',
                                                  time_range = input$inputTerm,
                                                  limit = 5) %>% 
        mutate(Position = row_number()) %>% 
        select(Position, name) %>% tibble()})
    
    top_5_tracks <- reactive({get_my_top_artists_or_tracks(type = 'tracks',
                                                            time_range = input$inputTerm,
                                                            limit = 5) %>% 
        mutate(Position = row_number()) %>% 
        select(Position, name) %>% tibble()})
    
    all_my_fav_tracks <-
      reactive({
        ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
          seq() %>%
          map(function(x) {
            get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
          }) %>% reduce(rbind)
      })
      
    
    
    artist_from_fav_tracks <- reactive({all_my_fav_tracks() %>%
        select(track.artists) %>%
        reduce(rbind) %>%
        reduce(rbind) %>%
        select(id, name)})
      
    
    track_num_artist <- reactive({artist_from_fav_tracks() %>%
        count(id, sort = TRUE) %>%
        left_join(artist_from_fav_tracks(), by = 'id',.) %>%
        unique() %>%
        select(-id) %>%
        top_n(20, n) %>% 
        arrange(desc(n))})
      
    
    output$top5artistsTable <- DT::renderDataTable(top_5_artists())
    
    output$top5tracksTable <- DT::renderDataTable(top_5_tracks())
    
    output$top10favouriteArtists <- DT::renderDataTable(track_num_artist())
    }


# # Run the application 
# shinyApp(ui = ui, server = server)
