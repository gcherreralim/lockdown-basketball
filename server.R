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
  observeEvent(input$TE_reset,{
    updateSelectizeInput(session, 'TEteams', selected = "")
    updateVarSelectInput(session, 'TExaxis', selected = "oEFF")
    updateVarSelectInput(session, 'TEyaxis', selected = "WinPerc")
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
      filter(Team != input$PTC_team,
             Conf %in% input$PTC_conf,
             OffDef == input$PTC_offdef)
    PTCotherteamsE
  })

  # Other Teams - Freq
  PTCotherperc = reactive({
    PTCotherteamsP = playtypePerc %>%
      filter(Team != input$PTC_team,
             Conf %in% input$PTC_conf,
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

  # # Z-Normalization Eff
  # PTC_zscoreEff = reactive({
  #   data.frame(scale(PTC_KNNeff()))
  # })
  # 
  # # Z-Normalization Freq
  # PTC_zscorePerc = reactive({
  #   data.frame(scale(PTC_KNNperc()))
  # })

  # KNN Eff
  PTC_KNNmatch_e = reactive({
    as.numeric(knnx.index(PTC_KNNeff(), PTC_KNNeff()[1, ,drop = FALSE], k = 6))
  })

  # KNN Freq
  PTC_KNNmatch_p = reactive({
    as.numeric(knnx.index(PTC_KNNperc(), PTC_KNNperc()[1, ,drop = FALSE], k = 6))
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

  PTC_PPPdata = reactive({
    PTC_match_e() %>%
    select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
  })
  
  PTC_PERCdata = reactive({
    PTC_match_p() %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
  })
  
  
  ### Graph
  # Eff Plot
  output$PTC_PPPplot = renderPlotly({
    validate(
      need(dim(PTCothereff())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )

    PTC_effplot = plot_ly(type = "scatterpolar",
                          mode = "closest",
                          fill = "toself") %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[1,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = PTC_match_e()[1,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[2,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[2,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[3,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[3,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[4,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[4,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[5,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_e()[5,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PPPdata()[6,])),
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

   PTC_percplot = plot_ly(type = "scatterpolar",
                          mode = "closest",
                          fill = "toself") %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[1,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = PTC_match_p()[1,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[2,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[2,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[3,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[3,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[4,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[4,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[5,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = PTC_match_p()[5,1]
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(PTC_PERCdata()[6,])),
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
    filter(TeamCode %in% PTC_match_e()$TeamCode) %>%
      filter((Team != input$PTC_team)&(Season != input$PTC_season))
  })

  output$PTC_PPPtable2 = renderReactable({
    reactable(playtypeMatchEff() %>%
                select(-TeamCode, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = FALSE, striped = TRUE, searchable = FALSE, defaultSorted = "Team", defaultSortOrder = "asc", filterable = TRUE,
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
      filter(TeamCode %in% PTC_match_p()$TeamCode) %>%
      filter((Team != input$PTC_team)&(Season != input$PTC_season))
  })

  output$PTC_PERCtable2 = renderReactable({
    reactable(playtypeMatchPerc() %>%
                select(-TeamCode, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = FALSE, striped = TRUE, searchable = FALSE, defaultSorted = "Team", defaultSortOrder = "asc", filterable = TRUE,
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

  observeEvent(input$PTC_reset,{
    updateSelectInput(session, 'PTC_team', selected = "MIL")
    updateSelectInput(session, 'PTC_season', selected = "2019-2020")
    updateCheckboxGroupInput(session, 'PTC_conf', selected = c("West" = "West", "East" = "East"))
    updateRadioButtons(session, 'PTC_offdef', selected = "offense")
  })
  ############## SERVER CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ################
  
  
  ############## SERVER CODE FOR '5-YEAR WINDOW ANALYSIS' TAB ################
  
  
  
}