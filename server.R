
authenticate <- function(id, secret) {
    # authenticate the spotify client stuff
    Sys.setenv(SPOTIFY_CLIENT_ID = id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
    
    access_token <- get_spotify_access_token()
}



server <- function(input, output, server) {
    observeEvent(input$btn, {if(authenticate(input$id, input$secret)!=''){
      sendSweetAlert(
        session = getDefaultReactiveDomain(),
        title = "Login Information",
        text = "Succesfully logged in",
        type = "success"
      )
    }else{
      sendSweetAlert(
        session = getDefaultReactiveDomain(),
        title = "Login Information",
        text = "Something goes wrong!",
        type = "error"
      )
    }})

    
    top_5_artists <-
      reactive({
        get_my_top_artists_or_tracks(type = 'artists',
                                     time_range = input$inputTerm,
                                     limit = 5) %>%
          mutate(Position = row_number()) %>% 
          select(Position, name) %>% 
          rename('Artist Name' = 'name') %>% 
          tibble()
      })
    
    top_5_tracks <-
      reactive({
        get_my_top_artists_or_tracks(type = 'tracks',
                                     time_range = input$inputTerm,
                                     limit = 5) %>%
          mutate(Position = row_number()) %>% 
          select(Position, name) %>% 
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
     
    
    dataset_tracks_with_features <- reactive({ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
        seq() %>%
        map(function(x) {
          get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
        }) %>% 
        reduce(rbind) %>% 
        tibble() %>% 
        select(added_at,track.artists,track.name,track.id) %>% 
        unnest() %>% 
        select(added_at, name, track.id, track.name) %>% 
        nest(name) -> dataset_tracks_names
      
      dataset_tracks_names %>% 
        mutate(artistsnames = map(.x = dataset_tracks_names$data, .f = ~paste(.x$name, collapse = ", "))) %>% 
        unnest() %>% select(-name) %>% 
        distinct() -> dataset_tracks_names_final
      
      
      features <- dataset_tracks_names %>% 
        select(track.id) %>% 
        lapply(get_track_audio_features) %>% 
        as.data.frame() %>% 
        select(track.id.danceability:track.id.tempo, track.id.id, track.id.duration_ms) %>% 
        rename_all(~str_replace(.,"^track.id.",""))
      
      dataset <- dataset_tracks_names_final %>% left_join(features, by = c("track.id" = "id"))}) 
    
    track_num_artist <- reactive({artist_from_fav_tracks() %>%
        count(id, sort = TRUE) %>%
        left_join(artist_from_fav_tracks(), by = 'id',.) %>%
        unique() %>%
        select(-id) %>%
        top_n(20, n)  %>% 
        rename('Artist_Name' = 'name', 'Quantity' = n) %>% 
        arrange(desc(Quantity)) %>% mutate(Position = row_number()) %>% tibble()
      })
      
    
    output$top5artistsTable <- renderPlotly({
      data1 <- top_5_artists()
      plot_ly(
      type = 'table',
      header = list(
        values = names(data1)
      ),
      cells = list(
        values = t(as.matrix(unname(data1)))
      )
    )
      })
    
    output$top5tracksTable <- renderPlotly({
      data2 <- top_5_tracks()
      plot_ly(
        type = 'table',
        header = list(
          values = names(data2)
        ),
        cells = list(
          values = t(as.matrix(unname(data2)))
        )
      )
    })
    
    
    output$top10favouriteArtist <- renderPlotly({
      data3 <- track_num_artist() 
      plot_ly(
        type = 'table',
        header = list(
          values = c('Position', 'Artist_Name', 'Quantity')
        ),
        cells = list(
          values = rbind(data3$Position, data3$Artist_Name, data3$Quantity)
        )
      )
    })
    
    
    output$CategoryPlot <- renderPlotly({
      data <- dataset_tracks_with_features() %>% 
        select(input$inputxAxis, input$inputyAxis, track.name, artistsnames) %>% 
        rename(xinput = input$inputxAxis, yinput = input$inputyAxis)
      p <- plot_ly(x = data$xinput,
                   y = data$yinput,
                   split = data$track.name,
                   type = 'scatter',
                   mode = 'markers'
                   ) %>% layout(xaxis = list(title = input$inputxAxis),
                                yaxis = list (title = input$inputyAxis))
        
    })
    
    output$plot_3dpca <- renderPlotly({
      
      spotify_df <- dataset_tracks_with_features()
      
      if(nrow(spotify_df)>0) {
        
        features = c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
        
        pca = prcomp(spotify_df[,features], scale = T)
        
        var_explained = sum((pca$sdev^2)[1:2])/sum(pca$sdev^2)
        
        rotation = pca$rotation %>% data.frame  %>% select(PC1, PC2, PC3)
        rotation$names = row.names(rotation)
        
        df_plot = cbind(pca$x, spotify_df[,c("track.name", "added_at", "artistsnames")])
        
        rotation2 = apply(rotation[,c("PC1", "PC2", "PC3", "names")], 2, function(x) rbind(c(0), x, c(NA))) %>% data.frame(stringsAsFactors = F)
        rotation2[which(rotation2$PC1==0),]$names = NA
        rotation2$PC1 = as.numeric(rotation2$PC1)
        rotation2$PC2 = as.numeric(rotation2$PC2)
        rotation2$PC3 = as.numeric(rotation2$PC3)
        
        # df_plot[,c("PC1", "PC2", "PC3")] = df_plot[,c("PC1", "PC2", "PC3")] %>% apply(2, function(x) (x - mean(x))/(max(x) - min(x)))
        
        df_plot[,c("PC1", "PC2", "PC3")] = df_plot[,c("PC1", "PC2", "PC3")]/max(df_plot[,c("PC1", "PC2", "PC3")])
        
        # grouped_by = ifelse(n_distinct(df_plot$artist_name)>1, "artist_name", "album_name")
        # 
        # names(df_plot)[names(df_plot) == grouped_by] = "grouped_by"
        
        set.seed(10)
        pal <- sample(brewer.pal(12, 'Paired'), n_distinct(spotify_df$artistsnames), replace = T)
        
        
        plot_ly(df_plot, x = ~PC1, y = ~PC2, z = ~PC3, colors = pal, text = ~paste("<b>Song:</b>", track.name)) %>%
          add_markers() %>%
          layout(scene = list(xaxis = list(title = 'PC1'),
                              yaxis = list(title = 'PC2'),
                              zaxis = list(title = 'PC3')),
                 legend = list(x = 0.5, y = 0, orientation = "h")) %>%
          add_trace(data = rotation2, x = ~PC1, y = ~PC2, z = ~PC3, color = "Components", text = ~paste("<b>Component:</b>", names), line = list(color = "#FFFFFF"), text = NULL, type = 'scatter3d', mode = "lines", width = 2) %>% 
          layout(paper_bgcolor='transparent')
      }
    })
    
    }


# # Run the application 
# shinyApp(ui = ui, server = server)
