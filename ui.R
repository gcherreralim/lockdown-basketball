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
                      overflow-x: hidden;
                    }
                    .navbar{
                      background-color: #17408B;
                      width: 103vw;
                      margin-left: -0.8vw;
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
                      background: #030303;
                      width: 104vw;
                      margin-top: -2vh;
                      margin-left: -1.65vw;
                      padding: 40px;
                      margin-bottom: 2vh;
                      background-size: cover;
                      background-repeat: no-repeat;
                      background-position: right;
                    }
                    .tooltabtitle > h1, .tooltabtitle > h4{
                      color: #FFF;
                      width: 40vw;
                    }
                    .tooltabtitle > h1{
                      text-transform: uppercase;
                      font-weight: 700;
                      letter-spacing: 2px;
                      font-size: 32px;
                    }
                    .tooltabtitle > h4{
                      font-size: 12px;
                      text-align: justify;
                      text-align-last: left;
                    }
                    #TE_tooltitle{
                      background-image: url("TE_head.jpg");
                    }
                    #PTC_tooltitle{
                      background-image: url("PTC_head.jpg");
                    }
                    #WA_tooltitle{
                      background-image: url("WA_head.jpg");
                    }
                    #MTC_tooltitle{
                      background-image: url("MTC_head.jpg");
                    }
                    #PTC_PPPplot{
                      display: block;
                      margin-left: auto;
                      margin-right: auto;
                    }
                    #PTC_FREQplot{
                      display: block;
                      margin-left: auto;
                      margin-right: auto;
                    }
                    #PTC_PERCplot{
                      display: block;
                      margin-left: auto;
                      margin-right: auto;
                    }
                    #WA_title1{
                      margin: 0 auto;
                    }
                    .MTC_plottitle > h5{
                      text-transform: uppercase;
                      font-weight: 800;
                      letter-spacing: 1px;
                      text-align: center;
                    }
                    .shiny-output-error-validation,
                    .htmlwidgets-error{
                      color: #C9082A;
                      font-size: 13px;
                      text-align: center;
                    }
                    #TEChartTitle{
                      font-size: 18px;
                      font-weight: 700;
                      text-align: center;
                    }
                    #WA_title1,
                    #WA_subtitle1,
                    #WA_subtitle2,
                    #WA_subtitle3,
                    #WA_subtitle4{
                      text-align: center;
                    }
                    #WA_title1{
                      font-size: 16px;
                      font-weight: 700;
                      text-transform: uppercase;
                      margin-top: 10px;
                      margin-bottom: 5px;
                    }
                    #WA_subtitle1,
                    #WA_subtitle2,
                    #WA_subtitle3,
                    #WA_subtitle4{
                      font-size: 12px;
                      font-weight: 700;
                      text-transform: uppercase;
                      color: #030303;
                    }
                    #tab-2846-5 > h4,
                    #tab-2846-5 > h6{
                      font-size: 14px
                    }
                    .PTCteamtitle{
                      font-weight: 600;
                      text-transform: uppercase;
                      font-size: 14px;
                      color: #FFF;
                      background-color: #062256;
                      padding: 5px;
                      border-radius: 5px;
                      text-align: center;
                    }
                    #PPPselectedteam > img,
                    #FREQselectedteam > img,
                    #PERCselectedteam > img,
                    #EDselectedteam > img,
                    #FDselectedteam > img,
                    #PDselectedteam > img{
                      margin-left: auto;
                      margin-right: auto;
                      display: block;
                    }
                    #WAtabsub > h4,
                    #WAtabsub > h6{
                      font-size: 12px;
                      text-align: center;
                      font-weight: 700;
                      color: #030303;
                    }
                    .matchcenter{
                      text-align: center;
                    }
                    .matchcenter > div{
                      margin-left: auto;
                      margin-right: auto;
                      display: block;
                    }
                    #main-text{
                      font-size: 20px;
                      width: 90vw;
                      padding-top: 20px;
                      padding-bottom: 60px;
                      margin: 0 auto;
                      text-align: center;
                      color: #FFF;
                      font-weight: 400;
                    }
                    #homepage{
                      margin-left:-2.5vw;
                      margin-top:-20px;
                      width:100vw;
                      height: 95vh;
                      background-color: #030303;
                    }
                    #aboutpage{
                      margin-left: -2.5vw;
                      margin-top: -20px;
                      width: 100vw;
                      height: 120vh;
                      background-color: #030303;
                    }
                    #main-image{
                      width: 102vw;
                    }
                    #main-head{
                      margin: 0 auto;
                      padding-top: 60px;
                      text-align: center;
                      font-size: 50px;
                      font-family: "Raleway";
                      font-weight: 800;
                      color: #FFF;
                      letter-spacing: 2px;
                    }
                    #homepage > h5{
                      color: #FFF;
                      margin: 0 auto;
                      text-align: center;
                    }
                    #homepage > h5 > a,
                    #brett-links>a,
                    #gabby-links>a{
                      color: #FFF;
                      text-decoration: none;
                      transition: all 350ms ease-in-out;
                    }
                    #homepage > h5 > a:hover,
                    #brett-links>a:hover,
                    #gabby-links>a:hover{
                      color: #C9082A;
                    }
                    #brett-links,
                    #gabby-links{
                      margin: 0 auto;
                      text-align: center;
                      padding-top: 10px;
                      padding-bottom: 50px;
                    }
                    #brett-head,
                    #gabby-head{
                      margin: 0 auto;
                      padding-top: 20px;
                      text-align: center;
                      font-size: 24px;
                      font-family: "Raleway";
                      font-weight: 800;
                      color: #FFF;
                      letter-spacing: 2px;
                      text-transform: uppercase;
                    }
                    #brett-text,
                    #gabby-text{
                      font-size: 16px;
                      width: 80vw;
                      padding-top: 20px;
                      padding-bottom: 10px;
                      margin: 0 auto;
                      text-align: center;
                      color: #FFF;
                      font-weight: 400;
                    }
                    #brett-pic,
                    #gabby-pic{
                      height: 350px;
                      margin-left: auto;
                      margin-right: auto;
                      display: block;
                      padding-top: 60px;
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
                 div(id="homepage",
                 #includeHTML("html_pages/home.html")
                 img(src="home-head.jpg", id="main-image"),
                 h1("LOCKDOWN BASKETBALL: NBA TEAM COMPS", id="main-head"),
                 h3("Inspired by a friend’s sports analytics final project, we set out to dive deeper into the similarities between NBA teams over the last five years.
                      The tools in this application will hopefully be both useful and entertaining in comparing multiple iterations of organizations and identifying trends across the NBA.
                      All of our data is regular season data, collected from both NBA.com/stats as well as NBAstuffer.com. We will be making updates that will include playoff data once
                      the 2019-2020 NBA season has fully concluded.", id="main-text"),
                 hr(),
                 h5("Brett Kornfeld (",tags$a(href="https://www.twitter.com/KornHoops", target="_blank", "@KornHoops"),")   |   Gabby Herrera-Lim (", tags$a(href="https://www.gcherreralim.com", target="_blank", "gcherreralim.com"),")")
                 )
               )),
      
      ########## UI CODE FOR 'TEAM EVALUATION' TAB ##########
      tabPanel("Team Evaluation",
               div(
                h1('Team Evaluation'),
                h4('This tool allows the user to graph up to ten teams against each other on a simple X-Y scatter plot based on any two variables of their choosing. 
                  The user can also choose to show all other teams in the data, while still labeling the already chosen teams.'),
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
                 div(style="display: inline-block; vertical-align: top; width: 200px; margin: 0 auto;",uiOutput("TE_plotdownappear"))
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
                             tabPanel("Playtype Efficiency (PPP)",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("PPPselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("PPPteam1")),
                                                 column(2,
                                                        uiOutput("PPPteam2")),
                                                 column(2,
                                                        uiOutput("PPPteam3")),
                                                 column(2,
                                                        uiOutput("PPPteam4")),
                                                 column(2,
                                                        uiOutput("PPPteam5"))
                                               )))),
                                      hr(),
                                      plotly::plotlyOutput("PTC_PPPplot",
                                                   height = "500px",
                                                   width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_PPPtable")),
                             tabPanel("Playtype Frequency",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("FREQselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("FREQteam1")),
                                                 column(2,
                                                        uiOutput("FREQteam2")),
                                                 column(2,
                                                        uiOutput("FREQteam3")),
                                                 column(2,
                                                        uiOutput("FREQteam4")),
                                                 column(2,
                                                        uiOutput("FREQteam5"))
                                               )))),
                                      hr(),
                                      plotly::plotlyOutput("PTC_FREQplot",
                                                   height = "500px",
                                                   width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_FREQtable")),
                             tabPanel("Playtype Percentile",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("PERCselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("PERCteam1")),
                                                 column(2,
                                                        uiOutput("PERCteam2")),
                                                 column(2,
                                                        uiOutput("PERCteam3")),
                                                 column(2,
                                                        uiOutput("PERCteam4")),
                                                 column(2,
                                                        uiOutput("PERCteam5"))
                                               )))),
                                      hr(),
                                      plotly::plotlyOutput("PTC_PERCplot",
                                                           height = "500px",
                                                           width = "700px"),
                                      shiny::hr(),
                                      reactable::reactableOutput("PTC_PERCtable")),
                             tabPanel("Efficiency Data",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("EDselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("EDteam1")),
                                                 column(2,
                                                        uiOutput("EDteam2")),
                                                 column(2,
                                                        uiOutput("EDteam3")),
                                                 column(2,
                                                        uiOutput("EDteam4")),
                                                 column(2,
                                                        uiOutput("EDteam5"))
                                               )))),
                                      hr(),
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Efficiency' tab."),
                                      reactable::reactableOutput("PTC_PPPtable2"),
                                      hr(),
                                      downloadButton('PTC_PPPtabledownload2',"Download data")),
                             tabPanel("Frequency Data",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("FDselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("FDteam1")),
                                                 column(2,
                                                        uiOutput("FDteam2")),
                                                 column(2,
                                                        uiOutput("FDteam3")),
                                                 column(2,
                                                        uiOutput("FDteam4")),
                                                 column(2,
                                                        uiOutput("FDteam5"))
                                               )))),
                                      hr(),
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Frequency' tab."),
                                      reactable::reactableOutput("PTC_FREQtable2"),
                                      hr(),
                                      downloadButton('PTC_FREQtabledownload2',"Download data")),
                             tabPanel("Percentile Data",
                                      fluidRow(
                                        column(2,
                                               h5("Selected Team:", class = "PTCteamtitle"),
                                               uiOutput("PDselectedteam")),
                                        column(10,
                                               h5("Matched Teams:", class = "PTCteamtitle"),
                                               div(class="matchcenter",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("PDteam1")),
                                                 column(2,
                                                        uiOutput("PDteam2")),
                                                 column(2,
                                                        uiOutput("PDteam3")),
                                                 column(2,
                                                        uiOutput("PDteam4")),
                                                 column(2,
                                                        uiOutput("PDteam5"))
                                               )))),
                                      hr(),
                                      h6("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Percentile' tab."),
                                      reactable::reactableOutput("PTC_PERCtable2"),
                                      hr(),
                                      downloadButton('PTC_PERCtabledownload2',"Download data")))))),

      # ########## UI CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ##########
      tabPanel("Multiple Team Comparisons",
               div(
                 h1('Multiple Team Comparisons'),
                 h4('Much like the Play Type Comparison Tab, this tool will similarly create spider charts related to frequency, efficiency, and percentile 
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
                                  fluidRow(
                                    column(4,
                                           div(h5("Offensive Frequency (%)"), class = "MTC_plottitle"),
                                           plotly::plotlyOutput("MTC_OffFreqPlot")),
                                    column(4,
                                           div(h5("Offensive Efficiency (PPP)"), class = "MTC_plottitle"),
                                           plotly::plotlyOutput("MTC_OffEffPlot")),
                                    column(4,
                                           div(h5("Offensive Percentile"), class = "MTC_plottitle"),
                                           plotly::plotlyOutput("MTC_OffPercPlot"))
                                  ))),
                         fluidRow(
                           column(12,
                                  shiny::br(),
                                  fluidRow(
                                    column(4,
                                           div(h5("Defensive Frequency (%)"), class = "MTC_plottitle"),
                                           plotly::plotlyOutput("MTC_DefFreqPlot")),
                                    column(4,
                                           div(h5("Defensive Efficiency (PPP)"), class = "MTC_plottitle"),
                                           plotly::plotlyOutput("MTC_DefEffPlot")),
                                    column(4,
                                           div(h5("Defensive Percentile"), class = "MTC_plottitle"),
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
                 class = "tooltabtitle", id = 'WA_tooltitle'),
               reactableOutput("WA_Table"),
               div(id="WAtabsub",
               h4("Select row above to see additional data"),
               h6('Defensive stats (Opponent PPG, Defensive Rating, Defensive Efficiency) are marked as positive (green) if they are below average, as lower numbers on defensive stats indicate better performance on the defensive end.'),
               ),
               hr(),
               div(class="WAplots",
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
               ))
               ),


      ########## UI CODE FOR 'ABOUT US' TAB ##########
      tabPanel("About Us",
               mainPanel(
                 div(id="aboutpage",
                     #includeHTML("html_pages/home.html")
                     div(id="aboutcontent",
                     img(src="brett-pic.jpg", id="brett-pic"),
                     h1("Brett Kornfeld (concept/structure)", id="brett-head"),
                     h3("A basketball (and sports in general) junkie in every fashion, Brett received his Master's of Business Analytics  from the Mendoza College of Business at the University of
                        Notre Dame in 2020. A graduate of Indiana University's Kelley School of Business in 2018, Brett is a two-time participant in Larry Coon's Sports Business Classroom at the 
                        NBA Summer League, specializing in both video/scouting and salary cap. He is actively seeking work in professional or collegiate men's or women's basketball on the analytics 
                        or operations side.", id="brett-text"),
                     h5(id="brett-links",
                        tags$a(href="https://www.twitter.com/KornHoops", target="_blank", "@KornHoops"),
                        br(),
                        br(),
                        tags$a(href="mailto:brettkornfeld@gmail.com", target="_blank", "E-mail Brett!")),
                     hr(),
                     img(src="gabby-pic.JPG", id="gabby-pic"),
                     h1("Gabby Herrera-Lim (design/app)", id="gabby-head"),
                     h3("Gabby graduated with degrees in Business Analytics (MS, University of Notre Dame) and Business Administration (BS, University of the Philippines, Diliman). He is currently an
                        economic analyst at the Asian Development Bank in the Philippines. In his spare time, he loves talking sports (especially basketball), designing content and apps, binging shows, 
                        and being a LeBron James apologist.", id="gabby-text"),
                     h5(id="gabby-links",
                        tags$a(href="https://gcherreralim.com", target="_blank", "Portfolio"),
                        br(),
                        br(),
                        tags$a(href="mailto:gcherreralim@alumni.nd.edu", target="_blank", "E-mail Gabby!")
                     )))
               ))
    )
  )
)

