shinyUI(fluidPage(theme = shinytheme("cyborg"),
                  navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Spotify User Analysis",
                    # Havbar 1, tabPanel
                    tabPanel("Intro",
                             sidebarPanel(
                               tags$h3("Input:"),
                               textInput("id", "Client ID: ", ""), # txt1 sent to the server
                               textInput("secret", "Client Secret: ", ""),    # txt2 sent to the server
                               actionButton("btn", "validate"),
                               textOutput("validate_message")
                             ), # sidebarPanel
                             mainPanel(
                               h1("Welcome to the Spotify User Analysis tool!"),
                               h6("Here you can see different analyses on your own music, as well as artists you follow, and what type of music you are interested in. Here are a couple first steps:"),
                               br(),
                               h6("Step 1: Go to https://developer.spotify.com/dashboard/ and login with your Spotify information"),
                               h6("Step 2: Create an app with name and description temp, then find the client ID and Client Secret"),
                               h6("Step 3: Copy and paste the ID and Secret into the designated dialog boxes, and click validate."),
                               h6("Step 4: Allow spotify to authenticate your account"),
                               h6("Now you should be good to go! Click one of the tabs above and learn more about your music"),
                               # h6("Step 4: When prompted with the message are you ..., make sure to click NOT YOU and login yourself. Now you're good to go! "),
                               # verbatimTextOutput("txtout"), # generated from the server
                             ) # mainPanel
                             
                             
                    ), # Navbar 2, tabPanel
                    tabPanel(
                      "Favourite Tracks and Artists",
                      sidebarPanel(
                        tags$h3("Time Range:"),
                        selectInput(
                          "inputTerm",
                          "Time Range",
                          c(
                            '4 weeks' = 'short_term',
                            '6 months' = 'medium_term',
                            'All history' = 'long_term'
                          )
                        ),fluidRow(img(src="anime.gif", style="display: block; margin-left: auto; margin-right: auto;"))
                      ),
                      # , height='250px',width='500px'
                      # sidebarPanel
                      mainPanel(
                        tags$style(HTML("
                  
                    thead {
                    color: #000000;
                    background-color: #5cc639;
                    }

                     tbody {
                    color: #000000;
                    background-color: #5cc639 !important;
                    }

                   "
                                        
                                        
                        )),
                        fluidRow(column(4, tags$h3(
                          "Top 5 Most Listened Artists"
                        ),offset = 1),
                        column(
                          4, tags$h3("Top 5 Most Listened Tracks"), offset = 2
                        )),
                        fluidRow(
                          column(4, DT::dataTableOutput("top5artistsTable"), offset = 1),
                          column(4, DT::dataTableOutput("top5tracksTable"), offset = 2)
                        ),
                        br(),
                        fluidRow(column(4, tags$h3(
                          "How often does the artist appear in liked songs?"
                        ),offset = 1),
                        column(
                          4, tags$h3("Top 5 Most Listened Tracks"), offset = 2
                        )),
                        fluidRow(column(
                          4, DT::dataTableOutput("top10favouriteArtists"), offset = 1
                        ),
                        column(4, "d", offset = 2))
                        
                        ,
                      )# mainPanel
                            
                    ), # Navbar 3, tabPanel
                    tabPanel("Recommendations",
                             sidebarPanel(
                               tags$h3("Choose statistics to analyze:"),
                               selectInput(
                                 "inputxAxis",
                                 "Category on X Axis",
                                 c(
                                   "Danceability"="danceability",
                                   "Energy"="energy",
                                   "Loudness"="loudness",
                                   "Speechiness"="speechiness",
                                   "Acousticness"="acousticness",
                                   "Instrumentalness"="instrumentalness" ,
                                   "Liveness"="liveness",
                                   "Valence"="valence",
                                   "Tempo"="tempo"
                                 )
                               ),
                               selectInput(
                                 "inputyAxis",
                                 "Category on Y Axis",
                                 c(
                                   "Danceability"="danceability",
                                   "Energy"="energy",
                                   "Loudness"="loudness",
                                   "Speechiness"="speechiness",
                                   "Acousticness"="acousticness",
                                   "Instrumentalness"="instrumentalness" ,
                                   "Liveness"="liveness",
                                   "Valence"="valence",
                                   "Tempo"="tempo"
                                 )
                               )
                             ),
                             mainPanel(
                               tags$h3("Let's look closer at statistics of every song that belongs to Liked Playlist"),
                               fluidRow(
                                 column(6, plotlyOutput("CategoryPlot"), offset = 3)
                               ) 
                               ))
                  ) 
                    
                 # ) # navbarPage
) # fluidPage
)