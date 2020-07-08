server <- function(input,output,session){
  
  ############## SERVER CODE FOR 'TEAM EVALUATION' TAB ################
  # Allow to select a name from the list of pre-existing names:
  observe({
    updateSelectizeInput(
      session = session,
      inputId = "TEteams",
      label = "Select up to 10 teams:",
      choices = list(
        "2015 - 2016" = sort(unique(genteams$TeamCode[genteams$Season == 2016])),
        "2016 - 2017" = sort(unique(genteams$TeamCode[genteams$Season == 2017])),
        "2017 - 2018" = sort(unique(genteams$TeamCode[genteams$Season == 2018])),
        "2018 - 2019" = sort(unique(genteams$TeamCode[genteams$Season == 2019])),
        "2019 - 2020" = sort(unique(genteams$TeamCode[genteams$Season == 2020]))),
      selected = NULL)
  })
  
  TE_out1 = reactive({
    validate(
      need(length(input$TEteams) > 1,
           "Please select at least two teams to see plot and table results!")
    )
    
    genteams %>%
      filter(TeamCode %in% input$TEteams)
  })
  
  TE_var1 = reactive({
    genteams %>%
      select(!!input$TExaxis)
  })
  
  TE_var2 = reactive({
    genteams %>%
      select(!!input$TEyaxis)
  })
  
  output$TEChartTitle = renderText({
    paste0(input$TExaxis, " - ", input$TEyaxis, " Comparison")
  })
  
  output$TEChart = renderPlot({
    req(input$TExaxis, input$TEyaxis)
    ggplot(TE_out1(), aes(x = !!input$TExaxis, y = !!input$TEyaxis,
                          xmax = max(TE_var1()), ymax = max(TE_var2()), 
                          color = name, fill = name)) +
      geom_point(shape = 21, size = 4, stroke = 2, show.legend = F) +
      ggrepel::geom_label_repel(aes(label = TeamCode), show.legend = F,
                                fontface = "bold",
                                box.padding = unit(0.55, "lines"),
                                point.padding = unit(0.55, "lines"),
                                segment.size = 1) +
      scale_fill_teams(2) +
      scale_color_teams(1) +
      theme(text = element_text(color = "#030303",
                                face = "bold"),
            panel.grid.major = element_line(colour = "#E4E4E4"),
            panel.grid.minor = element_line(color = "#E4E4E4"),
            panel.background = element_rect(fill = 'white'),
            axis.ticks = element_blank())
  })
  
  output$TEtable = renderReactable({
    reactable(TE_out1(), pagination = FALSE, striped = TRUE, searchable = FALSE, defaultSorted = "WinPerc", defaultSortOrder = "desc",
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                Team = colDef(show = FALSE),
                TeamCode = colDef(name = "Team"),
                Div = colDef(name = "Division"),
                Season = colDef(show = FALSE),
                SeasonRange = colDef(name = "Season"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct"),
                EstWinPerc = colDef(show = FALSE),
                ProjWinPerc = colDef(show = FALSE),
                AchLevel = colDef(show = FALSE),
                AvgPTDiff = colDef(name = "Diff"),
                EFFDiff = colDef(name = "eDiff"),
                ASTPerc = colDef(name = "AST%"),
                ORebPerc = colDef(name = "OREB%"),
                DRebPerc = colDef(name = "DREB%"),
                RebPerc = colDef(name = "REB%"),
                TOVPerc = colDef(name = "TOV%"),
                eFGPercSeason = colDef(name = "eFG"),
                TSPerc = colDef(name = "TS%"),
                Playoff = colDef(show = FALSE),
                name = colDef(show = FALSE),
                primary = colDef(show = FALSE),
                secondary = colDef(show = FALSE)
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  ############## SERVER CODE FOR 'PLAY TYPE COMPARISONS' TAB ################
  # Selected Team - Eff
  PTCteameff = reactive({
    playtypeEff %>%
      filter(Team == input$PTC_team,
             SeasonRange == input$PTC_season,
             OffDef == input$PTC_offdef)
  })
  # Selected Team - Freq
  PTCteamperc = reactive({
    playtypePerc %>%
      filter(Team == input$PTC_team,
             SeasonRange == input$PTC_season,
             OffDef == input$PTC_offdef)
  })
  
  # Other Teams - Eff
  PTCothereff = reactive({
    PTCotherteamsE = playtypeEff %>%
      filter(Conf %in% input$PTC_conf,
             OffDef == input$PTC_offdef)
    PTCotherteamsE
  })
  
  # Other Teams - Freq
  PTCotherperc = reactive({
    PTCotherteamsP = playtypePerc %>%
      filter(Conf %in% input$PTC_conf,
             OffDef == input$PTC_offdef)
    PTCotherteamsP
  })
  
  # Merge - Eff
  PTCallEff = reactive({
    rbind(PTCteameff(), PTCothereff())
  })
  
  # Merge - Freq
  PTCallPerc = reactive({
    rbind(PTCteamperc(), PTCotherperc())
  })
  
  # KNN Data - Eff
  PTC_KNNeff = reactive({
    KNNeff = PTCallEff() %>%
      select(8:17)
    KNNeff
  })
  
  # KNN Data - Freq
  PTC_KNNperc = reactive({
    KNNperc = PTCallPerc() %>%
      select(8:17)
    KNNperc
  })
  
  # Z-Normalization Eff
  PTC_zscoreEff = reactive({
    data.frame(scale(PTC_KNNeff()))
  })
  
  # Z-Normalization Freq
  PTC_zscorePerc = reactive({
    data.frame(scale(PTC_KNNperc()))
  })
  
  # KNN Eff
  PTC_KNNmatch_e = reactive({
    as.numeric(knnx.index(PTC_zscoreEff(), PTC_zscoreEff()[1, drop = FALSE], k = 6))
  })
  
  # KNN Freq
  PTC_KNNmatch_p = reactive({
    as.numeric(knnx.index(PTC_zscorePerc(), PTC_zscorePerc()[1, drop = FALSE], k = 6))
  })
  
  # TEAM MATCHES
  PTC_match_e = reactive({
    allmatch_e = PTCallEff()[PTC_KNNmatch_e(),]
    selteam_e = allmatch_e[1,]
    othermatch_e = allmatch_e[-1,] %>%
      arrange(TeamCode)
    combine_e = rbind(selteam_e, othermatch_e)
  })
  
  PTC_match_p = reactive({
    allmatch_p = PTCallPerc()[PTC_KNNmatch_p(),]
    selteam_p = allmatch_p[1,]
    othermatch_p = allmatch_p[-1,] %>%
      arrange(TeamCode)
    combine_p = rbind(selteam_p, othermatch_p)
  })
  
  ### Graph
  # Eff Plot
  output$PTC_PPPplot = renderPlotly({
    validate(
      need(dim(PTCothereff())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )
    
    PTC_PPPdata = PTC_match_e() %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    PTC_effplot = plot_ly(type = "scatterpolar",
                          mode = "closest",
                          fill = "toself")
    
    PTC_effplot = PTC_effplot %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[1,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = PTC_match_e()[1,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[2,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[2,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[3,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[3,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[4,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[4,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[5,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[5,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PPPdata[6,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[6,1] 
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,2))),
        showlegend = TRUE)
    PTC_effplot
  })
  
  # Eff Table
  output$PTC_PPPtable = renderReactable({
    reactable(PTC_match_e() %>%
                select(Team, Conf, Div, SeasonRange, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
              pagination = FALSE, striped = TRUE, searchable = FALSE,
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                Div = colDef(name = "Division"),
                SeasonRange = colDef(name = "Season")
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  # Freq Plot
  output$PTC_PERCplot = renderPlotly({
    validate(
      need(dim(PTCotherperc())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )
    
    PTC_PERCdata = PTC_match_p() %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    PTC_percplot = plot_ly(type = "scatterpolar",
                          mode = "closest",
                          fill = "toself")
    
    PTC_percplot = PTC_percplot %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[1,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = PTC_match_p()[1,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[2,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[2,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[3,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[3,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[4,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[4,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[5,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[5,1] 
      ) %>%
      add_trace(
        r = as.matrix(PTC_PERCdata[6,]),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[6,1] 
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100))),
        showlegend = TRUE)
    PTC_percplot
  })
  
  # Freq Table
  output$PTC_PERCtable = renderReactable({
    reactable(PTC_match_p() %>%
                select(Team, Conf, Div, SeasonRange, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
              pagination = FALSE, striped = TRUE, searchable = FALSE,
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                Div = colDef(name = "Division"),
                SeasonRange = colDef(name = "Season")
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  # Full Tables
  playtypeMatchEff = reactive({
    playtypes %>%
    filter(TeamCode %in% PTC_match_e()$TeamCode)
  })
  
  output$PTC_PPPtable2 = renderReactable({
    reactable(playtypeMatchEff() %>%
                select(-TeamCode, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = FALSE, striped = TRUE, searchable = FALSE,
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                Div = colDef(name = "Division"),
                SeasonRange = colDef(name = "Season"),
                Conf = colDef(name = "Conference"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct")
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  playtypeMatchPerc = reactive({
    playtypes %>%
      filter(TeamCode %in% PTC_match_p()$TeamCode)
  })
  
  output$PTC_PERCtable2 = renderReactable({
    reactable(playtypeMatchPerc() %>%
                select(-TeamCode, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = FALSE, striped = TRUE, searchable = FALSE,
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                Div = colDef(name = "Division"),
                SeasonRange = colDef(name = "Season"),
                Conf = colDef(name = "Conference"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct")
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  
  ############## SERVER CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ################
  
  
  ############## SERVER CODE FOR '5-YEAR WINDOW ANALYSIS' TAB ################
  
  
  
}

# # Server
# server <- function(input, output, session){
#   
#   # Selected Team
#   selected_team <- reactive({
#     teams_table %>% 
#       filter(Team_Year == input$team)
#   })
#   
#   # All Other Teams
#   all_other_teams <- reactive({
#     year <- as.numeric(str_extract(input$team, '[0-9]+'))
#     all_teams <- teams_table %>% 
#       filter(Team_Year != input$team) %>% 
#       filter(Avg_Height >= input$height[1]) %>% 
#       filter(Avg_Height <= input$height[2]) %>% 
#       filter(Overall_Rank >= input$overall_rank[1]) %>% 
#       filter(Overall_Rank <= input$overall_rank[2]) %>% 
#       filter(Year >= (as.numeric(str_extract(input$team, "[0-9]+")) - as.numeric(input$years_before))) %>% 
#       filter(Year <= (as.numeric(str_extract(input$team, "[0-9]+")) + as.numeric(input$years_after))) %>% 
#       filter(Conference %in% input$conference)
#     all_teams
#   })
#   
#   # Merged teams 
#   all_teams_macthing_filters <- reactive({
#     rbind(selected_team(), all_other_teams())
#   })
#   
#   # KNN Data 
#   KNN_Data <- reactive({
#     table <- all_teams_macthing_filters() %>% 
#       select(6:41)
#     table
#   })
#   
#   # Z-Score Normalization ASK ERIC
#   z_score_data <- reactive({
#     data.frame(scale(KNN_Data()))
#   })
#   
#   # KNN
#   KNN_Matches <- reactive({
#     as.numeric(knnx.index(z_score_data(), z_score_data()[1, ,drop=FALSE], k=11))
#   })
#   
#   # KNN Team Matches with Info
#   
#   KNN_Matches_Info <- reactive({
#     all_teams <- all_teams_macthing_filters()[KNN_Matches(),]
#     selected_team <- all_teams[1, ]
#     ten_other_teams <- all_teams[-1, ] %>% 
#       arrange(Unique_Identifier)
#     combined <- rbind(selected_team, ten_other_teams)
#     combined
#   })
#   
#   # Map
#   arena_data <- reactive({
#     original_team <- merge(selected_team(), team_arena, by.x="Unique_Identifier", by.y = "unique_id") %>% 
#       select(Team, Arena, Capacity, City, State, Latitude, Longitude, Overall_Rank, Wins, Loss, Year, Conference, Logos)
#     other_teams <- merge(KNN_Matches_Info()[-1,], team_arena, by.x="Unique_Identifier", by.y = "unique_id") %>% 
#       select(Team, Arena, Capacity, City, State, Latitude, Longitude, Overall_Rank, Wins, Loss, Year, Conference, Logos)
#     arenas <- rbind(original_team, other_teams)
#     team_arena_data <- arenas %>% 
#       mutate(popup = str_c(str_c("School: ", Team, sep = ""),
#                            str_c("Season: ", Year, sep =""),
#                            str_c("Arena: ", Arena, sep = ""),
#                            str_c("Capacity: ", Capacity, sep = ""),
#                            str_c("Ken Pom Rank: ", Overall_Rank, sep = ""),
#                            str_c("Record: ", Wins, "-", Loss, sep =""),
#                            str_c("Conference: ", Conference, sep = ""),
#                            str_c("City: ", City, sep = ""),
#                            str_c("State: ", State, sep = ""),
#                            sep = "<br/>"))
#     team_arena_data
#   })
#   
#   output$NCAA_Map <- renderLeaflet({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     ncaa_map <- leaflet(arena_data()) %>% 
#       addProviderTiles("CartoDB.Positron")  %>% 
#       setView(-98.35, 39.7,
#               zoom = 4) %>% 
#       addMarkers(~Longitude, ~Latitude,
#                  popup = ~popup)
#     ncaa_map
#   })
#   
#   # Graphs and Table Section
#   
#   # Graph
#   output$four_factors_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     four_factors_data <- KNN_Matches_Info() %>% 
#       select(Opp_eFG, Opp_TO, Def_Reb, Opp_FT_Rate,
#              Off_eFG, Off_TO, Off_Reb, Off_FT_Rate)
#     
#     four_factors_plot <-  plot_ly(type = "scatterpolar",
#                                   mode = "closest",
#                                   fill = "toself")
#     four_factors_plot <- four_factors_plot %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[1,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         name = KNN_Matches_Info()[1,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[2,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[2,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[3,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[3,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[4,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[4,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[5,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[5,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[6,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[6,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[7,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[7,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[8,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[8,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[9,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[9,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[10,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[10,1]) %>% 
#       add_trace(
#         r = as.matrix(four_factors_data[11,]),
#         theta = c("Opp eFG", "Opp TO", "Def Reb", "Opp FT Rate",
#                   "Off eFG", "Off TO", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible="legendonly",
#         name = KNN_Matches_Info()[11,1]) %>% 
#       layout(
#         polar = list(
#           radialaxis = list(
#             visible = T,
#             range = c(0,100))),
#         showlegend=TRUE)
#     four_factors_plot
#   })
#   
#   # Four Factors Table
#   output$four_factors_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     four_factors_data <- KNN_Matches_Info() %>% 
#       select(Team_Year, Opp_eFG, Opp_TO, Def_Reb, Opp_FT_Rate,
#              Off_eFG, Off_TO, Off_Reb, Off_FT_Rate)
#     four_factors_data
#   }))
#   
#   # Advanced Statistics
#   # Advanced Stats Plot
#   output$advanced_stats_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     # Numeric Plots
#     numeric_data <- KNN_Matches_Info() %>% 
#       select(Strength_of_Schedule_Opp_D, Strength_of_Schedule_Opp_O, AdjO, AdjD, AdjT, Avg_Height, Bench, Continuity)
#     
#     # Numeric Plots
#     numeric_plot <- plot_ly(type = "scatterpolar",
#                             mode = "closest",
#                             fill = "toself")
#     
#     numeric_plot <- numeric_plot %>% 
#       add_trace(
#         r = as.matrix(numeric_data[1,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         name = KNN_Matches_Info()[1,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[2,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[2,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[3,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[3,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[4,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[4,1]) %>%   
#       add_trace(
#         r = as.matrix(numeric_data[5,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[5,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[6,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[6,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[7,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[7,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[8,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[8,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[9,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[9,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[10,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[10,1]) %>% 
#       add_trace(
#         r = as.matrix(numeric_data[11,]),
#         theta = c("Opp Defense", "Opp Offense", "AdjO", "AdjD", "AdjT", "Avg Height", "Bench", "Continuity"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[11,1]) %>% 
#       layout(
#         polar = list(
#           radialaxis = list(
#             visible = T,
#             range = c(0,140))))
#     numeric_plot
#   })
#   
#   
#   # Advanced Statistics Table
#   output$advanced_stats_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     numeric_data <- KNN_Matches_Info() %>% 
#       select(Team_Year, Strength_of_Schedule_Opp_D, Strength_of_Schedule_Opp_O, AdjO, AdjD, AdjT, Avg_Height, Bench, Continuity)
#     numeric_data
#   }))
#   
#   # Offense
#   # Offense Graph 
#   output$advanced_offense_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     offense_data <- KNN_Matches_Info() %>% 
#       select(Off_3P, Off_2P, Off_FT, Off_A, Off_3P_Attempts, Off_eFG, Off_Reb, Off_FT_Rate)
#     
#     offense_plot <- plot_ly(type = "scatterpolar",
#                             mode = "closest",
#                             fill = "toself")
#     
#     offense_plot <- offense_plot %>% 
#       add_trace(
#         r = as.matrix(offense_data[1,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         name = KNN_Matches_Info()[1,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[2,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[2,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[2,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[2,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[3,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[3,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[4,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[4,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[5,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[5,1]) %>%   
#       add_trace(
#         r = as.matrix(offense_data[6,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[6,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[7,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[7,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[8,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[8,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[9,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[9,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[10,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[10,1]) %>% 
#       add_trace(
#         r = as.matrix(offense_data[11,]),
#         theta = c("Off 3P", "Off 2P", "Off FT", "Off A", "Off 3P Attempts", "Off eFG", "Off Reb", "Off FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[11,1]) %>% 
#       layout(
#         polar = list(
#           radialaxis = list(
#             visible = T,
#             range = c(0,100))))
#     offense_plot
#   })  
#   # Offense Statistics Table
#   output$offense_stats_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     offense_data <- KNN_Matches_Info() %>% 
#       select(Team_Year, Off_3P, Off_2P, Off_FT, Off_A, Off_3P_Attempts, Off_eFG, Off_Reb, Off_FT_Rate, Off_Stl, Off_NST, Off_Blk)
#   }))
#   
#   # Defense
#   # Defense Plot 
#   output$defense_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     defense_data <- KNN_Matches_Info() %>% 
#       select(Opp_3P, Opp_2P, Opp_FT, Opp_A, Opp_3P_Attempts, Opp_eFG, Def_Reb, Opp_FT_Rate)
#     
#     defense_plot <- plot_ly(type = "scatterpolar",
#                             mode = "closest",
#                             fill = "toself")
#     
#     defense_plot <- defense_plot %>% 
#       add_trace(
#         r = as.matrix(defense_data[1,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         name = KNN_Matches_Info()[1,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[2,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[2,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[3,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[3,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[4,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[4,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[5,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[5,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[6,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[6,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[7,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[7,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[8,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[8,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[9,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[9,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[10,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[10,1]) %>% 
#       add_trace(
#         r = as.matrix(defense_data[11,]),
#         theta = c("Opp 3P", "Opp 2P", "Opp FT", "Opp A", "Opp 3P Attempts", "Opp eFG", "Def Reb", "Opp FT Rate"),
#         showlegend = TRUE,
#         mode = "markers",
#         visible = "legendonly",
#         name = KNN_Matches_Info()[11,1]) %>% 
#       layout(
#         polar = list(
#           radialaxis = list(
#             visible = T,
#             range = c(0,100))))
#     defense_plot
#   })
#   
#   # Defense Table
#   output$defense_team_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     defense_data <- KNN_Matches_Info() %>% 
#       select(Team_Year, Opp_3P, Opp_2P, Opp_FT, Opp_A, Opp_3P_Attempts, Opp_eFG, Def_Reb, Opp_FT_Rate, Defense_Stl, Opp_NST, Defense_Blk)
#     defense_data
#   }))
#   
#   # Luck Plot
#   output$luck_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     luck_plot <- ggplot(KNN_Matches_Info(), aes(Team_Year, Luck)) +
#       geom_bar(stat="identity", color = "dodgerblue3", fill = "dodgerblue3") + theme_classic() + coord_flip() +
#       labs(x = "", y = "Luck")
#   })
#   
#   # Experience Plot 
#   output$experience_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     experience_plot <- ggplot(KNN_Matches_Info(), aes(Team_Year, Experience)) +
#       geom_bar(stat="identity", color = "dodgerblue3", fill = "dodgerblue3") + theme_classic() + coord_flip() +
#       labs(x= "", y = "Average Years in School")
#   })
#   
#   # Offense small Data
#   offense_small_stats <- reactive({
#     KNN_Matches_Info() %>% 
#       select(Team_Year, Off_Stl, Off_NST, Off_Blk)
#   })
#   
#   offense_melt <- reactive({
#     melt(offense_small_stats(), id.vars="Team_Year")
#   })
#   
#   output$offense_small_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     small_off_plot <- ggplot(offense_melt(), aes(Team_Year, value)) +
#       geom_bar(stat="identity", position="dodge", aes(fill = variable)) + theme_classic() + coord_flip() +
#       labs(x = "", y = "Rate of Occurrence") + 
#       scale_fill_manual(values = c("skyblue1", "slategray4", "steelblue4"),
#                         name = "Category",
#                         labels =c("Off Stl", "Off NST", "Off Blk"))
#   })
#   
#   # Defense Small Stats
#   defense_small_stats <- reactive({
#     KNN_Matches_Info() %>% 
#       select(Team_Year, Defense_Stl, Opp_NST, Defense_Blk)
#   })
#   
#   defense_melt <- reactive({
#     melt(defense_small_stats(), id.vars="Team_Year")
#   })
#   
#   output$defense_small_plot <- renderPlotly({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     ggplot(defense_melt(), aes(Team_Year, value)) +
#       geom_bar(stat="identity", position="dodge", aes(fill = variable)) + theme_classic() + coord_flip() +
#       labs(x = "", y = "Rate of Occurrence") + 
#       scale_fill_manual(values = c("skyblue1", "slategray4", "steelblue4"),
#                         name = "Category",
#                         labels =c("Def Stl", "Opp NST", "Def Blk"))
#   })
#   
#   # Selected Team table 
#   output$selected_team_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     selected_team() %>% 
#       select(-Unique_Identifier)
#   }))
#   
#   # Matches Team Table
#   
#   output$match_team_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     KNN_Matches_Info()[-1,] %>% 
#       select(-Unique_Identifier)
#   }))
#   
#   #Match characteristics 
#   output$match_characteristics <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Sorry, the required number of matches was not met.  
#            Please change the input filters."
#       )
#     )
#     
#     all_teams_macthing_filters()
#   }))
#   
#   # All Data Table
#   output$all_data_table <- DT::renderDataTable(DT::datatable({
#     
#     validate(
#       need(input$conference_table != 0, "Please select a conference."
#       )
#     )
#     
#     table <- teams_table
#     table <- table %>% 
#       filter(Year >= input$year_table[1]) %>% 
#       filter(Year <= input$year_table[2]) %>% 
#       filter(Overall_Rank >= input$overall_rank_table[1]) %>% 
#       filter(Overall_Rank <= input$overall_rank_table[2])
#     if (input$conference_table != 'All') {
#       table <- table %>% 
#         filter(Conference %in% input$conference_table)
#     }
#     table
#   }))
#   
#   # Conferences Select 
#   observe({
#     if (input$selectall > 0) {
#       if (input$selectall %% 2 == 0){
#         updateCheckboxGroupInput(session = session,
#                                  inputId="conference",
#                                  choices=c("A10"="A10", "ACC"="ACC", "AE"="AE", "Amer"="Amer",
#                                            "ASun"="ASun", "B10"="B10", "B12"="B12", "BE"="BE",
#                                            "BSky"="BSky", "BSth"="BSth", "BW"="BW",
#                                            "CAA"="CAA", "CUSA"="CUSA", "GWC" ="GWC", "Horz"="Horz", "ind"="ind",
#                                            "Ivy"="Ivy", "MAAC"="MAAC", "MAC"="MAC", "Mcon"="Mcon", 
#                                            "MEAC"="MEAC", "MVC"="MVC", "MWC"="MWC", "NEC"="NEC",
#                                            "OVC"="OVC", "P10"="P10", "P12"="P12", "Pat"="Pat",
#                                            "SB"="SB", "SC"="SC", "SEC"="SEC", "Slnd"="Slnd",
#                                            "Sum"="Sum", "SWAC"="SWAC", "WAC"="WAC", "WCC"="WCC"),
#                                  selected = c("A10", "ACC", "AE", "Amer", "ASun", "B10", "B12", "BE",
#                                               "BSky", "BSth", "BW", "CAA", "CUSA", "GWC", "Horz", "ind",
#                                               "Ivy", "MAAC", "MAC", "Mcon", "MEAC", "MVC", "MWC", "NEC",
#                                               "OVC", "P10", "P12", "Pat", "SB", "SC", "SEC", "Slnd",
#                                               "Sum", "SWAC", "WAC", "WCC"),
#                                  inline = TRUE)
#         
#       }
#       else {
#         updateCheckboxGroupInput(session =session,
#                                  inputId="conference",
#                                  choices=c("A10"="A10", "ACC"="ACC", "AE"="AE", "Amer"="Amer",
#                                            "ASun"="ASun", "B10"="B10", "B12"="B12", "BE"="BE",
#                                            "BSky"="BSky", "BSth"="BSth", "BW"="BW",
#                                            "CAA"="CAA", "CUSA"="CUSA", "GWC" ="GWC", "Horz"="Horz", "ind"="ind",
#                                            "Ivy"="Ivy", "MAAC"="MAAC", "MAC"="MAC", "Mcon"="Mcon", 
#                                            "MEAC"="MEAC", "MVC"="MVC", "MWC"="MWC", "NEC"="NEC",
#                                            "OVC"="OVC", "P10"="P10", "P12"="P12", "Pat"="Pat",
#                                            "SB"="SB", "SC"="SC", "SEC"="SEC", "Slnd"="Slnd",
#                                            "Sum"="Sum", "SWAC"="SWAC", "WAC"="WAC", "WCC"="WCC"),
#                                  selected = c(),
#                                  inline=TRUE)
#         
#       }}
#   })
#   
#   # Text 
#   output$selected_team_year <- renderText({
#     KNN_Matches_Info()$Year
#   })
#   
#   # Images
#   # Selected Team 
#   output$selected_team_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[1],
#              height = 45)
#   })
#   
#   output$selected_team <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[1],
#              height = 55)
#   })
#   
#   output$selected_team_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[1],
#              height = 80)
#   })
#   
#   # Team 1
#   output$team_1_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[2],
#              height = 45)
#   })
#   
#   output$team_1 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[2],
#              height = 55)
#   })
#   
#   output$team_1_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[2],
#              height = 80)
#   })
#   
#   # Team 2 
#   output$team_2_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[3],
#              height = 45)
#   })
#   
#   output$team_2 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[3],
#              height = 55)
#   })
#   
#   output$team_2_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[3],
#              height = 80)
#   })
#   
#   # Team 3
#   output$team_3_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[4],
#              height = 45)
#   })
#   
#   output$team_3 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[4],
#              height = 55)
#   })
#   
#   output$team_3_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[4],
#              height = 80)
#   })
#   
#   # Team 4
#   output$team_4_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[5],
#              height = 45)
#   })
#   
#   output$team_4 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[5],
#              height = 55)
#   })
#   
#   output$team_4_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[5],
#              height = 80)
#   })
#   
#   # Team 5 
#   output$team_5_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[6],
#              height = 45)
#   })
#   
#   output$team_5 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[6],
#              height = 55)
#   })
#   
#   output$team_5_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[6],
#              height = 80)
#   })
#   
#   # Team 6 
#   output$team_6_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[7],
#              height = 45)
#   })
#   
#   output$team_6 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[7],
#              height = 55)
#   })
#   
#   output$team_6_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[7],
#              height = 80)
#   })
#   
#   # Team 7
#   output$team_7_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[8],
#              height = 45)
#   })
#   
#   output$team_7 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[8],
#              height = 55)
#   })
#   
#   output$team_7_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[8],
#              height = 80)
#   })
#   
#   # Team 8 
#   output$team_8_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[9],
#              height = 45)
#   })
#   
#   output$team_8 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[9],
#              height = 55)
#   })
#   
#   output$team_8_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[9],
#              height = 80)
#   })
#   
#   # Team 9 
#   output$team_9_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[10],
#              height = 45)
#   })
#   
#   output$team_9 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[10],
#              height = 55)
#   })
#   
#   output$team_9_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[10],
#              height = 80)
#   })
#   
#   # Team 10 
#   output$team_10_home <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[11],
#              height = 45)
#   })
#   
#   output$team_10 <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[11],
#              height = 55)
#   })
#   
#   output$team_10_map <-  renderUI({
#     
#     validate(
#       need(dim(all_other_teams())[1]>=10, "Need 10 Teams"
#       )
#     )
#     
#     tags$img(src = arena_data()$Logos[11],
#              height = 80)
#   })
# }