
authenticate <- function(id, secret) {
    # authenticate the spotify client stuff
    Sys.setenv(SPOTIFY_CLIENT_ID = id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
    
    access_token <- get_spotify_access_token()
}



server <- function(input, output) {
    validate <- observeEvent(input$btn, {authenticate(input$id, input$secret)})
    output$validate_message <- renderText({
      if (validate()) {
        "Succesfully logged in"
      }
      else {
        "Something goes wrong!"
      }
    }) 
    
    top_5_artists <-
      reactive({
        get_my_top_artists_or_tracks(type = 'artists',
                                     time_range = input$inputTerm,
                                     limit = 5) %>%
          select(name) %>% 
          rename('Artist Name' = 'name') %>% 
          tibble()
      })
    
    top_5_tracks <-
      reactive({
        get_my_top_artists_or_tracks(type = 'tracks',
                                     time_range = input$inputTerm,
                                     limit = 5) %>%
          select(name) %>% 
          rename('Track Name' = 'name') %>% 
          tibble()
      })
    
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
        rename('Artist Name' = 'name', 'Quantity' = n) %>% 
        arrange(desc(Quantity))})
      
    
    output$top5artistsTable <-
      DT::renderDataTable(
        top_5_artists() %>% DT::datatable(.) %>% DT::formatStyle(
          0:nrow(top_5_artists()),
          color = "white",
          backgroundColor = "#5cc639"
        )
      )
    
    output$top5tracksTable <-
      DT::renderDataTable(
        top_5_tracks() %>% DT::datatable(.) %>% DT::formatStyle(
          0:nrow(top_5_artists()),
          color = "white",
          backgroundColor = "#5cc639"
        )
      )
    
    output$top10favouriteArtists <-
      DT::renderDataTable(
        track_num_artist() %>% DT::datatable(.) %>% DT::formatStyle(
          0:nrow(top_5_artists()),
          color = "white",
          backgroundColor = "#5cc639"
        )
      )
    
    output$CategoryPlot <- renderPlotly({
      p <- features[[1]][[1]] %>%
        select(input$inputxAxis, input$inputyAxis, id) %>%
        ggplot(aes_string(input$inputxAxis, input$inputyAxis, color = "id")) +
        geom_point() +
        theme_bw()
      ggplotly(p)
    })
    
    }


# # Run the application 
# shinyApp(ui = ui, server = server)
