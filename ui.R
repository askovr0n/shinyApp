shinyUI(
  fluidPage(#theme = shinytheme("cyborg"),
    setBackgroundColor(
      color = c("#30372f", "#000000"),
      gradient = "radial",
      direction = c("top", "right")
    ),
    
    tags$head(tags$style(
      HTML(
        '.navbar-static-top  {
        background: rgb(99,255,0); 
        background: linear-gradient(201deg, rgba(29,185,84,1) 0%, rgba(9,121,20,1) 50%, rgba(0,0,0,1) 100%);
        border-radius: 16px;
        box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
        backdrop-filter: blur(7px);
        -webkit-backdrop-filter: blur(7px);
        border: 1px solid rgba(62, 208, 36, 1);
        }
        .navbar-default .navbar-brand {
        color: #FFFFFF;
        }
        .navbar-default .navbar-nav>li>a {
        color: #FFFFFF;
        }
        h1, h2, h3, h4, h5, h6 {
        color: #FFFFFF;
        }
        a {
        color: #FFFFFF;
        }
        .well {
        background: rgba(29, 185, 84, 0.48);
        border-radius: 16px;
        box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
        backdrop-filter: blur(7px);
        -webkit-backdrop-filter: blur(7px);
        border: 1px solid rgba(62, 208, 36, 1);
        }
        #id-label, #secret-label, #inputTerm-label, #inputxAxis-label, #inputyAxis-label{
        color: #FFFFFF;
        }
        '
      )
    )),
    
            useShinyjs(),
            useSweetAlert(),
    shiny::navbarPage(
     
    title = div("Spotify User Analysis",tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://www.spotify.com/pl/\"><img src=\"spot3.png\" alt=\"alt\" style=\"float:right;width:47px;height:50px;padding-top:2px;\"> </a></div>');
    console.log(header)")
    )),
                    # Havbar 1, tabPanel
                    
    tabPanel("Intro",
                             sidebarPanel(
                               tags$h3("Provide your login data:"),
                               textInput("id", "Client ID: ", ""), # txt1 sent to the server
                               textInput("secret", "Client Secret: ", ""),    # txt2 sent to the server
                               actionButton("btn", "validate"),
                               br(),
                               # textOutput("validate_message")
                             ), # sidebarPanel
                             mainPanel(
                               h1("Welcome to the Spotify User Analysis tool!"),
                               h4("Here you can see different analyses on your own music, as well as artists you follow, and what type of music you are interested in. Here are a couple first steps:"),
                               br(),
                               h4("Step 1: Go to https://developer.spotify.com/dashboard/ and login with your Spotify information"),
                               h4("Step 2: Create an app with name and description temp, then find the client ID and Client Secret"),
                               h4("Step 3: Copy and paste the ID and Secret into the designated dialog boxes, and click validate."),
                               h4("Step 4: Allow spotify to authenticate your account"),
                               h4("Now you should be good to go! Click one of the tabs above and learn more about your music"),
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
                        
                        fluidRow(column(4, tags$h3(
                          "Top 5 Most Listened Artists"
                        ),offset = 1),
                        column(
                          4, tags$h3("Top 5 Most Listened Tracks"), offset = 2
                        )),
                        fluidRow(
                          column(4, plotlyOutput("top5artistsTable"), offset = 1),
                          column(4, plotlyOutput("top5tracksTable"), offset = 2)
                        ),
                        br(),
                        fluidRow(column(4, tags$h3(
                          "How often does the artist appear in liked songs?"
                        ),offset = 1),
                        column(
                          4, tags$h3("Top 5 Most Listened Tracks"), offset = 2
                        )),
                        fluidRow(column(
                          4, plotlyOutput("top10favouriteArtists"), offset = 1
                        ),
                        column(4, "d", offset = 2))
                        
                        ,
                      )# mainPanel
                            
                    ), # Navbar 3, tabPanel
                    tabPanel("Analysis of Liked Songs",
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
                               ), fluidRow(img(src="pepe2.gif", style="display: block; margin-left: auto; margin-right: auto;", height='260px',width='498px'))
                             ),
                             mainPanel(
                               tags$h3("Let's look closer at statistics of every song that belongs to Liked Playlist"),
                               fluidRow(
                                 column(12, plotlyOutput("CategoryPlot"))
                               ) 
                               )),
    tabPanel("Recommendations",
             mainPanel(
      tags$h3("Let's look closer at statistics of every song that belongs to Liked Playlist"),
      fluidRow(
        column(12, plotlyOutput("plot_3dpca"))
      ) 
    ))
    # a(href='https://www.spotify.com/pl/',img(src='spotify.jpg', style = 'max-width: 10%; width: 10%, height: auto')),
                  ) 
                    
                 # ) # navbarPage
) # fluidPage
)