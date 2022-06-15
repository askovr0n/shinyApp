shinyUI(fluidPage(
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
                    tabPanel("Popularity",
                             sidebarPanel(
                               tags$h3("Time Range:"),
                               selectInput("inputTerm", "Time Range", c('4 weeks'='short_term', '6 months'='medium_term', 'All history'='long_term'))
                             ), # sidebarPanel
                             mainPanel(fluidRow(column(5, DT::dataTableOutput("top5artistsTable")), 
                                                column(5, DT::dataTableOutput("top5tracksTable"), offset = 2)),
                                       br(),
                                       fluidRow(column(5, DT::dataTableOutput("top10favouriteArtists")), 
                                                column(5, "d", offset = 2))
                               
                               ,
                             )# mainPanel
                            
                    ), # Navbar 3, tabPanel
                    tabPanel("Recommendations"
                             
                             ) 
                    ) 
                    
                 # ) # navbarPage
) # fluidPage
)