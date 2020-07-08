ui <- (
  fluidPage(
    tags$head(HTML('<link rel="icon", href="nba-logo-transparent.png",
                   type="image/png"/>
                   <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@400;500;600;700;900&display=swap" rel="stylesheet">')),
    
    ############# CSS CHUNKS ################
    # tags$style(HTML('')),
    
    
    ############# THEME ##############
    theme = shinytheme("yeti"),
    navbarPage(
      selected = "Home",
      title = "NBA TEAM COMPARISONS",
      windowTitle = "NBA Team Comparisons | 2019-20 Season",
      
      ########## UI CODE FOR 'HOME' TAB ##########
      tabPanel("Home",
               mainPanel(
                 #includeHTML("html_pages/home.html"),
                 h1("Hello")
               )),
      
      ########## UI CODE FOR 'TEAM EVALUATION' TAB ##########
      tabPanel("Team Evaluation",
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
                            br(),
                            varSelectInput("TExaxis", "X-Axis Variable", genteams[,7:36], selected="oEFF"),
                            varSelectInput("TEyaxis", "Y-Axis Variable", genteams[,7:36], selected="WinPerc")),
               mainPanel(width = 10,
                 textOutput("TEChartTitle"),
                 plotOutput("TEChart"),
                 reactableOutput("TEtable")
               )),
      #,
      
      
      ########## UI CODE FOR 'PLAY TYPE COMPARISONS' TAB ##########
      tabPanel("Play Type Comparisons",
               fluidRow(
                 column(2,
                        fluidRow(h5("Selected Team")),
                        fluidRow(uiOutput("PTCselectedteam"))),
                 column(10,
                        fluidRow(h5("Matched Teams:")),
                        fluidRow(
                          column(1,
                                 uiOutput("PTC_team1")),
                          column(1,
                                 uiOutput("PTC_team2")),
                          column(1,
                                 uiOutput("PTC_team3")),
                          column(1,
                                 uiOutput("PTC_team3")),
                          column(1,
                                 uiOutput("PTC_team4")),
                          column(1,
                                 uiOutput("PTC_team5"))))),
               br(),
               sidebarLayout( 
                 sidebarPanel(width = 3,
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
                              checkboxGroupInput("PTC_conf", "Conference:",
                                                 choices = unique(playtypes$Conf),
                                                 selected = c("West" = "West",
                                                              "East" = "East"),
                                                 inline = TRUE),
                              radioButtons("PTC_offdef", "Possession Type",
                                           choices = unique(playtypes$OffDef),
                                           selected = "offense"),
                              actionButton("PTC_reset", "Reset")),
                 mainPanel(width = 9,
                           tabsetPanel(
                             tabPanel("Points Per Possession",
                                      h3("Points Per Possession"),
                                      plotlyOutput("PTC_PPPplot"),
                                      hr(),
                                      reactableOutput("PTC_PPPtable")),
                             tabPanel("Playtype Percentile",
                                      h3("Playtype Percentile"),
                                      plotlyOutput("PTC_PERCplot"),
                                      hr(),
                                      reactableOutput("PTC_PERCtable")),
                             tabPanel("Efficiency (PPP) Data",
                                      h3("Efficiency Full Table"),
                                      h4("These are the playtype (all 10) numbers for the teams matched in the 'Points Per Possession' tab."),
                                      reactableOutput("PTC_PPPtable2")),
                             tabPanel("Percentile Data",
                                      h3("Percentile Full Table"),
                                      h4("These are the playtype (all 10) numbers for the teams matched in the 'Playtype Percentile' tab."),
                                      reactableOutput("PTC_PERCtable2"))))))#,

      # ########## UI CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ##########
      # tabPanel("Multiple Team Comparisons",
      #          sidebarPanel(width = 2,
      #                       selectizeInput(
      #                         inputId = "MTC_teams",
      #                         label = "Select up to 5 teams to compare:",
      #                         choices = fullteams$TeamCode,
      #                         multiple = TRUE,
      #                         selected = NULL,
      #                         options = list(maxItems = 5))),
      #          mainPanel(width = 8,
      #                    fluidRow(
      #                      column(12,
      #                             "Multiple Team Playtype Comparisons",
      #                             fluidRow(
      #                               column(4,
      #                                      "Offensive Freq",
      #                                      plotlyOutput("MTC_OffFreqPlot",
      #                                                   width = 400,
      #                                                   height = 350)),
      #                               column(4,
      #                                      "Offensive Points per Poss",
      #                                      plotlyOutput("MTC_OffEffPlot",
      #                                                   width = 400,
      #                                                   height = 350)),
      #                               column(4,
      #                                      "Offensive Percentile",
      #                                      plotlyOutput("MTC_OffPercPlot",
      #                                                   width = 400,
      #                                                   height = 350))
      #                             ))),
      #                    fluidRow(
      #                      column(12,
      #                             br(),
      #                             fluidRow(
      #                               column(4,
      #                                      "Defensive Freq",
      #                                      plotlyOutput("MTC_DefFreqPlot",
      #                                                   width = 400,
      #                                                   height = 350)),
      #                               column(4,
      #                                      "Defensive Points per Poss",
      #                                      plotlyOutput("MTC_DefEffPlot",
      #                                                   width = 400,
      #                                                   height = 350)),
      #                               column(4,
      #                                      "Defensive Percentile",
      #                                      plotlyOutput("MTC_DefPercPlot",
      #                                                   width = 400,
      #                                                   height = 350)))
      #                             )),
      #                    hr(),
      #                    reactableOutput("MTC_SumTable"))
      #                       ),
      # 
      # 
      # ########## UI CODE FOR '5-YEAR WINDOW ANALYSIS' TAB ##########
      # tabPanel("5-Year Window Analysis",
      #          h4("Select row to see additional data"),
      #          reactableOutput("WA_Table"),
      #          hr(),
      #          fluidRow(
      #            column(3,
      #                   plotOutput("WA_plot1")),
      #            column(3,
      #                   plotOutput("WA_plot2")),
      #            column(3,
      #                   plotOutput("WA_plot3")),
      #            column(3,
      #                   plotOutput("WA_plot4"))
      #          ),
      #          plotOutput("WA_trialplot")),
      # 
      # 
      # ########## UI CODE FOR 'ABOUT US' TAB ##########
      # tabPanel("About Us",
      #          mainPanel(
      #            includeHTML("html_pages/about.html")
      #          ))
    )
  )
)

