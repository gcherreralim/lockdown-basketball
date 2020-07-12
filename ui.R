ui <- (
  fluidPage(
    tags$head(HTML('<link rel="icon", href="nba-logo-transparent.png",
                   type="image/png"/>
                   <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@400;500;600;700;900&display=swap" rel="stylesheet">
                   <link href="https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;600;800&display=swap" rel="stylesheet">
                   <link href="https://fonts.googleapis.com/css2?family=Paytone+One&display=swap" rel="stylesheet">
                   <link href="https://fonts.googleapis.com/css2?family=Raleway:wght@200;400;500;700;900&display=swap" rel="stylesheet">')),
    
    ############# CSS CHUNKS ################
    tags$style(HTML('* { margin:0; padding:0;}
                    body{
                      font-family: "Open Sans", sans-serif;
                      font-size: 12px;
                    }
                    .navbar{
                      background-color: #17408B;
                      width: 100vw;
                      margin-left: -0.78vw;
                      font-weight: 500;
                      font-family: "Raleway", sans-serif;
                      border-color: transparent;
                    }
                    .navbar-default .navbar-brand, .navbar-default .navbar-brand:hover{
                      color: #FFF;
                      text-transform: uppercase;
                      letter-spacing: 3px;
                      font-weight: 600;
                      font-size: 12px;
                    }
                    .navbar-default .navbar-nav>li>a {
                      font-size: 10px;
                      color: #FFF;
                      letter-spacing: 1.5px;
                      border-bottom: 1px solid #17408B;
                      transition: all 200ms ease-in-out;
                    }
                    .navbar-default .navbar-nav>li>a:hover{
                      background-color: #C9082A;
                      color: #FFF;
                      border-bottom: 1px solid #c9082a;
                    }
                    .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
                      background-color: #C9082A;
                      color: #FFF;
                      text-transform: uppercase;
                      font-weight: 600;
                      border-bottom: 1px solid #c9082a;
                    }
                    hr{
                      height: 4px;
                      border: none;
                      background-color: #FFF;
                    }
                    .btn{
                      background-color: #17408B;
                      border: 3px solid #17408B;
                      color: #FFF;
                      font-weight: 400;
                      text-transform: uppercase;
                      transition: all 200ms ease-in-out;
                    }
                    .btn:hover{
                      background-color: #FFF;
                      border: 3px solid #17408B;
                      color: #17408B;
                      font-weight: 600;
                    }
                    .well{
                      background-color: #C9082A;
                      color: #FFF;
                    }
                    h1, h2, h3, h4, h5{
                      font-family: "Open Sans", sans-serif;
                      font-weight: 600;
                    }
                    .control-label{
                      font-weight: 400;
                    }
                    
                    /* REACTABLE */
                    .reactable > div > div.rt-table > div.rt-thead.-header > div {
                      background-color: #17408B;
                    }
                    .reactable > div > div.rt-table > div.rt-thead.-header > div > div.rt-align-center{
                      padding: 8px;
                      border-bottom-width: 1px;
                      background-color: #17408B;
                      text-transform: uppercase;
                      font-size: 11px;
                      color: #FFF;
                      transition: box-shadow 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275)
                    }
                    .reactable > div > div.rt-table > div.rt-thead.-header > div > div.rt-align-center:hover {
                      background-color: #C9082A;
                      color: #FFF;
                    }
                    .reactable > div > div.rt-table > div.rt-thead.-header > div > div.rt-align-center[aria-sort="ascending"]{
                      background-color: #C9082A;
                      color: #FFF;
                      box-shadow: inset 0 10px 0 -6px #EDB439;
                    }
                    .reactable > div > div.rt-table > div.rt-thead.-header > div > div.rt-align-center[aria-sort="descending"]{
                      background-color: #C9082A;
                      color: #FFF;
                      box-shadow: inset 0 -10px 0 -6px #EDB439;
                    }
                    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                      background-color: #C9082A;
                      color: #FFF;
                      border-radius: 0px;
                      border-color: #C9082A;
                      font-weight: 600;
                    }
                    .nav-tabs>.active>a, .nav-tabs>.active>a:hover, .nav-tabs>.active>a:focus {
                      border-color: transparent;
                      font-weight: 600;
                    }
                    .nav-tabs {
                      border-bottom: 8px solid #062256;
                      background-color: #17408B;
                    }
                    .nav-tabs > li > a{
                      transition: all 200ms ease-in-out;
                    }
                    .nav-tabs > li:hover > a{
                      background-color: #C9082A;
                      border-color: transparent;
                    }
                    .nav-tabs > li > a{
                      color: #FFF;
                      border-radius: 0px;
                      transition: all 200ms ease-in-out;
                    }
                    .tooltabtitle {
                      background-image: url("TE_head.jpg");
                      width: 125vw;
                      margin-top: -2vh;
                      margin-left: -1.55vw;
                      padding: 40px;
                      overflow: hidden;
                      margin-bottom: 2vh;
                    }
                    .tooltabtitle > h1, .tooltabtitle > h4{
                      color: #FFF;
                      width: 40vw;
                    }
                    .tooltabtitle > h1{
                      text-transform: uppercase;
                      font-weight: 700;
                    }
                    ')),
    
    
    ############# THEME ##############
    theme = NULL,
    navbarPage(
      selected = "Home",
      title = "NBA Team Comparisons",
      windowTitle = "NBA Team Comparisons 2019-20 | Lockdown Basketball",
      
      ########## UI CODE FOR 'HOME' TAB ##########
      tabPanel("Home",
               mainPanel(
                 #includeHTML("html_pages/home.html"),
                 h1("Hello")
               )),
      
      ########## UI CODE FOR 'TEAM EVALUATION' TAB ##########
      tabPanel("Team Evaluation",
               div(
                h1('Team Evaluation'),
                h4('This tool allows the user to graph up to ten teams against each other on a simple X-Y scatter plot based on any two variables of their choosing. 
                  The user can also choose to show all other teams in the data, while still distinguishing the already chosen teams.'),
                class = "tooltabtitle", id = 'TE_tooltitle'),
               sidebarPanel(width = 2,
                            selectizeInput(
                              inputId = "TEteams",
                              label = "Select up to 10 teams:",
                              choices = list(
                                "2015 - 2016" = sort(unique(genteams$TeamCode[genteams$Season == 2016])),
                                "2016 - 2017" = sort(unique(genteams$TeamCode[genteams$Season == 2017])),
                                "2017 - 2018" = sort(unique(genteams$TeamCode[genteams$Season == 2018])),
                                "2018 - 2019" = sort(unique(genteams$TeamCode[genteams$Season == 2019])),
                                "2019 - 2020" = sort(unique(genteams$TeamCode[genteams$Season == 2020]))),
                              multiple = TRUE,
                              selected = NULL,
                              options = list(maxItems = 10)),
                            shiny::br(),
                            varSelectInput("TExaxis", "X-Axis Variable", genteams[,7:36], selected="oEFF"),
                            varSelectInput("TEyaxis", "Y-Axis Variable", genteams[,7:36], selected="WinPerc"),
                            checkboxInput("TEall", "Show all teams", FALSE),
                            actionButton("TE_reset", "Reset")),
               mainPanel(width = 10,
                 shiny::textOutput("TEChartTitle"),
                 shiny::plotOutput("TEChart"),
                 reactable::reactableOutput("TEtable"),
                 br(),
                 br(),
                 div(style="display: inline-block; vertical-align: top; width: 200px; margin: 0 auto;",uiOutput("TE_plotdownappear")),
                 div(style="display: inline-block; vertical-align: top; width: 200px; margin: 0 auto; padding: 0px 20px;",uiOutput("TE_tabdownappear")),
                 br(),
               ))
      ,
      
      
      ########## UI CODE FOR 'PLAY TYPE COMPARISONS' TAB ##########
      tabPanel("Play Type Comparisons",
               div(
                 h1('Play Type Comparisons'),
                 h4('This tool is designed to help locate and identify teams of a similar profile over the last five regular seasons using publicly available NBA tracking data. 
                  The first three tabs visualize a K-Nearest Neighbor analysis that nets you the five closest teams within the selected parameters. The last three tabs display
                  relevant play type data for each of the teams identified as closest matches, including statistics for each play type.'),
                 class = "tooltabtitle", id = 'PTC_tooltitle'),
               # fluidRow(
               #   column(2,
               #          fluidRow(h5("Selected Team")),
               #          fluidRow(uiOutput("PTCselectedteam"))),
               #   column(10,
               #          fluidRow(h5("Matched Teams:")),
               #          fluidRow(
               #            column(1,
               #                   uiOutput("PTC_team1")),
               #            column(1,
               #                   uiOutput("PTC_team2")),
               #            column(1,
               #                   uiOutput("PTC_team3")),
               #            column(1,
               #                   uiOutput("PTC_team3")),
               #            column(1,
               #                   uiOutput("PTC_team4")),
               #            column(1,
               #                   uiOutput("PTC_team5"))))),
               # br(),
               sidebarLayout(
                 sidebarPanel(width = 2,
                              h5("SELECT A TEAM:"),
                              hr(),
                              selectInput("PTC_team", "Team:",
                                          choices = list(
                                            "East" = sort(unique(playtypes$Team[playtypes$Conf == "East"])),
                                            "West" = sort(unique(playtypes$Team[playtypes$Conf == "West"]))
                                            ),
                                          selectize = TRUE,
                                          selected = "MIL"),
                              selectInput("PTC_season", "Season:",
                                          unique(playtypes$SeasonRange),
                                          selectize = TRUE,
                                          selected = "2019-2020"),
                              hr(),
                              h5("MATCHING PARAMETERS:"),
                              hr(),
                              checkboxGroupInput("PTC_conf", "Conference:",
                                                 choices = unique(playtypes$Conf),
                                                 selected = c("West" = "West",
                                                              "East" = "East"),
                                                 inline = TRUE),
                              radioButtons("PTC_offdef", "Possession Type:",
                                           choices = c("Offense" = "offense",
                                                       "Defense" = "defense"),
                                           selected = "offense",
                                           inline = TRUE),
                              actionButton("PTC_reset", "Reset")),
                 mainPanel(width = 10,
                           tabsetPanel(
                             tabPanel("Playtype Efficiency",
                                      h3("Points Per Possession"),
                                      plotly::plotlyOutput("PTC_PPPplot",
                                                   height = "500px",
                                                   width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_PPPtable")),
                             tabPanel("Playtype Frequency",
                                      h3("Frequency"),
                                      plotly::plotlyOutput("PTC_FREQplot",
                                                   height = "500px",
                                                   width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_FREQtable")),
                             tabPanel("Playtype Percentile",
                                      h3("Percentile"),
                                      plotly::plotlyOutput("PTC_PERCplot",
                                                           height = "500px",
                                                           width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_PERCtable")),
                             tabPanel("Efficiency Data",
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Efficiency' tab."),
                                      reactable::reactableOutput("PTC_PPPtable2"),
                                      hr(),
                                      downloadButton('PTC_PPPtabledownload2',"Download data")),
                             tabPanel("Frequency Data",
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Frequency' tab."),
                                      reactable::reactableOutput("PTC_FREQtable2"),
                                      hr(),
                                      downloadButton('PTC_FREQtabledownload2',"Download data")),
                             tabPanel("Percentile Data",
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Percentile' tab."),
                                      reactable::reactableOutput("PTC_PERCtable2"),
                                      hr(),
                                      downloadButton('PTC_PERCtabledownload2',"Download data")))))),

      # ########## UI CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ##########
      tabPanel("Multiple Team Comparisons",
               div(
                 h1('Multiple Team Playtype Comparisons'),
                 h4('Much like the Playtype Comparison Tab, this tool will similarly create spider charts related to frequency, efficiency, and percentile 
                    via the 10 main playtypes. The user has the option to select up to five teams and overlay these offensive and defensive profiles to their preference.'),
                 class = "tooltabtitle", id = 'MTC_tooltitle'),
               sidebarPanel(width = 2,
                            selectizeInput(
                              inputId = "MTC_teams",
                              label = "Select up to 5 teams to compare:",
                              choices = list(
                                "2015 - 2016" = sort(unique(playtypes$TeamCode[playtypes$Season == 2016])),
                                "2016 - 2017" = sort(unique(playtypes$TeamCode[playtypes$Season == 2017])),
                                "2017 - 2018" = sort(unique(playtypes$TeamCode[playtypes$Season == 2018])),
                                "2018 - 2019" = sort(unique(playtypes$TeamCode[playtypes$Season == 2019])),
                                "2019 - 2020" = sort(unique(playtypes$TeamCode[playtypes$Season == 2020]))),
                              multiple = TRUE,
                              selected = NULL,
                              options = list(maxItems = 5)),
                            actionButton("MTC_reset", "Reset Teams")),
               mainPanel(width = 10,
                         fluidRow(
                           column(12,
                                  h3("Multiple Team Playtype Comparisons"),
                                  fluidRow(
                                    column(4,
                                           "Offensive Frequency (%)",
                                           plotly::plotlyOutput("MTC_OffFreqPlot")),
                                    column(4,
                                           "Offensive Efficiency",
                                           plotly::plotlyOutput("MTC_OffEffPlot")),
                                    column(4,
                                           "Offensive Percentile",
                                           plotly::plotlyOutput("MTC_OffPercPlot"))
                                  ))),
                         fluidRow(
                           column(12,
                                  shiny::br(),
                                  fluidRow(
                                    column(4,
                                           "Defensive Frequency (%)",
                                           plotly::plotlyOutput("MTC_DefFreqPlot")),
                                    column(4,
                                           "Defensive Efficiency",
                                           plotly::plotlyOutput("MTC_DefEffPlot")),
                                    column(4,
                                           "Defensive Percentile",
                                           plotlyOutput("MTC_DefPercPlot")))
                                  )),
                         h5("Double click a team name in each graph's legend to display all teams. Note that graphs will overlap."),
                         shiny::hr(),
                         reactable::reactableOutput("MTC_SumTable"))
                            )
      ,


      # ########## UI CODE FOR '5-YEAR WINDOW ANALYSIS' TAB ##########
      tabPanel("5-Year Window Analysis",
               div(
                 h1('5-Year Window Analysis'),
                 h4('This tool allows users to pick any team from any season and look at how they compared across all of the variables in our team evaluation tool, both against 
                    season average and average over the "five-year" window.'),
                 class = "tooltabtitle", id = 'MTC_tooltitle'),
               h4("Select row to see additional data"),
               h6('Defensive stats (Opponent PPG, Defensive Rating, Defensive Efficiency) are marked as positive (green) if they are below average, as lower numbers on defensive stats indicate better performance on the defensive end.'),
               reactableOutput("WA_Table"),
               downloadButton('WA_tabledownload',"Download the data"),
               hr(),
               uiOutput("WA_titleappear1"),
               fluidRow(
                 column(3,
                        uiOutput("WA_subtitleappear1"),
                        uiOutput("WA_plotappear1")),
                 column(3,
                        uiOutput("WA_subtitleappear2"),
                        uiOutput("WA_plotappear2")),
                 column(3,
                        uiOutput("WA_subtitleappear3"),
                        uiOutput("WA_plotappear3")),
                 column(3,
                        uiOutput("WA_subtitleappear4"),
                        uiOutput("WA_plotappear4"))
               )
               ),


      ########## UI CODE FOR 'ABOUT US' TAB ##########
      tabPanel("About Us",
               mainPanel(
                 #includeHTML("html_pages/about.html")
                 h3("About Us!")
               ))
    )
  )
)

