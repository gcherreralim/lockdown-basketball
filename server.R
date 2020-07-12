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
    genteams %>%
      filter(TeamCode %in% input$TEteams)
  })
  
  TE_out2 = reactive({
    genteams
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
  
  TEChartOut = reactive({
    
    req(input$TExaxis, input$TEyaxis)
    
    if (!input$TEall) {
      ggplot(TE_out1(), aes(x = !!input$TExaxis, y = !!input$TEyaxis,
                            xmax = max(TE_var1()), ymax = max(TE_var2()), 
                            color = name, fill = name)) +
        geom_point(shape = 21, size = 4, stroke = 1, show.legend = F) +
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
    } else {
      ggplot(TE_out2(), aes(x = !!input$TExaxis, y = !!input$TEyaxis,
                            xmax = max(TE_var1()), ymax = max(TE_var2()), 
                            color = name, fill = name)) +
        geom_point(shape = 21, size = 4, stroke = 1, show.legend = F) +
        ggrepel::geom_label_repel(data = TE_out2() %>%
                                    filter(TeamCode %in% input$TEteams),
                                  aes(label = TeamCode), show.legend = F,
                                  fontface = "bold",
                                  box.padding = unit(2, "lines"),
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
    }
  })
  
  
  output$TEChart = renderPlot({
    validate(
      need(length(input$TEteams) > 1,
           "Please select at least two teams to see plot and table results!")
    )
    
    req(TEChartOut())
    TEChartOut()
  })
  

  output$TEtable = renderReactable({
    validate(
      need(length(input$TEteams) > 1,
           "")
    )
    
    reactable(TE_out1(), pagination = FALSE, striped = FALSE, searchable = FALSE, defaultSorted = as.character(input$TE), defaultSortOrder = "desc",
              selection = "single", onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
              ),
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
                WinPerc = colDef(name = "W%"),
                EstWinPerc = colDef(show = FALSE),
                ProjWinPerc = colDef(show = FALSE),
                AchLevel = colDef(show = FALSE),
                SchedAdjRTG = colDef(name = "SchedARtg"),
                Consistency = colDef(name = "Cons"),
                AvgPTDiff = colDef(name = "Diff"),
                EFFDiff = colDef(name = "eDiff"),
                ASTPerc = colDef(name = "AST%"),
                ORebPerc = colDef(name = "OREB%"),
                DRebPerc = colDef(name = "DREB%"),
                RebPerc = colDef(name = "REB%"),
                TOVPerc = colDef(name = "TOV%"),
                eFGPercSeason = colDef(name = "eFG%"),
                TSPerc = colDef(name = "TS%"),
                Playoff = colDef(show = FALSE),
                name = colDef(show = FALSE),
                primary = colDef(show = FALSE),
                secondary = colDef(show = FALSE),
                .selection = colDef(show = FALSE)
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  observeEvent(input$TE_reset,{
    updateSelectizeInput(session, 'TEteams', selected = "")
    updateVarSelectInput(session, 'TExaxis', selected = "oEFF")
    updateVarSelectInput(session, 'TEyaxis', selected = "WinPerc")
    updateCheckboxInput(session, "TEall", value = FALSE)
  })
  
  output$TE_plotdownappear = renderUI({
    if(length(input$TEteams)>=2){
      downloadButton('TE_graphdownload', "Download the graph")
    }
  })
  
  output$TE_graphdownload = downloadHandler(
    filename = function(){
      paste0(as.character(input$TExaxis),"-",as.character(input$TEyaxis),' Comparison Graph (',paste(input$TEteams,collapse=","), ').png')
    },
    content = function(graphfile){
      req(TEChartOut())
      ggsave(graphfile, plot = TEChartOut(), device = 'png')
    }
  )
  
  output$TE_tabdownappear = renderUI({
    if(length(input$TEteams)>=2){
      downloadButton('TE_tabledownload',"Download the data")
    }
  })
  
  output$TE_tabledownload <- downloadHandler(
    filename = function() {
      paste0(as.character(input$TExaxis),"-",as.character(input$TEyaxis),' Comparisons (',paste(input$TEteams,collapse=","), ').csv')
    },
    content = function(file) {
      TEtabledown1 = TE_out1() %>%
        select(-Team, -Season, -EstWinPerc, -ProjWinPerc, -AchLevel, -Playoff, -name, -primary, -secondary)
      
      write.csv(TEtabledown1, file, row.names = FALSE)
    }
  )
  
  ############## SERVER CODE FOR 'PLAY TYPE COMPARISONS' TAB ################
  # Selected Team - Eff
  PTCteameff = reactive({
    playtypeEff %>%
      filter(Team == input$PTC_team,
             SeasonRange == input$PTC_season,
             OffDef == input$PTC_offdef)
  })
  
  # Selected Team - Freq
  PTCteamfreq = reactive({
    playtypeFreq %>%
      filter(Team == input$PTC_team,
             SeasonRange == input$PTC_season,
             OffDef == input$PTC_offdef)
  })
  
  # Selected Team - Perc
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
  PTCotherfreq = reactive({
    PTCotherteamsF = playtypeFreq %>%
      filter(Team != input$PTC_team,
             Conf %in% input$PTC_conf,
             OffDef == input$PTC_offdef)
    PTCotherteamsF
  })

  # Other Teams - Perc
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
  PTCallFreq = reactive({
    rbind(PTCteamfreq(), PTCotherfreq())
  })

  # Merge - Perc
  PTCallPerc = reactive({
    rbind(PTCteamperc(), PTCotherperc())
  })

  # KNN Data - Eff
  PTC_KNNeff = reactive({
    if (input$PTC_offdef == "offense") {
      
      KNNeff = PTCallEff() %>%
        select(8:17)
      KNNeff
    } else {
      KNNeff = PTCallEff() %>%
        select(9:17)
      KNNeff
    }
  })
  
  # KNN Data - Freq
  PTC_KNNfreq = reactive({
    if (input$PTC_offdef == "offense") {
      
      KNNfreq = PTCallFreq() %>%
        select(8:17)
      KNNfreq
    } else {
      KNNfreq = PTCallFreq() %>%
        select(9:17)
      KNNfreq
    }
  })

  # KNN Data - Perc
  PTC_KNNperc = reactive({
    if (input$PTC_offdef == "offense") {
      
      KNNfreq = PTCallPerc() %>%
        select(8:17)
      KNNfreq
    } else {
      KNNfreq = PTCallPerc() %>%
        select(9:17)
      KNNfreq
    }
  })

  # KNN Eff
  PTC_KNNmatch_e = reactive({
    as.numeric(knnx.index(PTC_KNNeff(), PTC_KNNeff()[1, ,drop = FALSE], k = 6))
  })
  
  # KNN Freq
  PTC_KNNmatch_f = reactive({
    as.numeric(knnx.index(PTC_KNNfreq(), PTC_KNNfreq()[1, ,drop = FALSE], k = 6))
  })

  # KNN Perc
  PTC_KNNmatch_p = reactive({
    as.numeric(knnx.index(PTC_KNNperc(), PTC_KNNperc()[1, ,drop = FALSE], k = 6))
  })

  # TEAM MATCHES
  PTC_match_e = reactive({
    allmatch_e = PTCallEff()[PTC_KNNmatch_e(),]
    selteam_e = allmatch_e[1,]
    othermatch_e = allmatch_e[-1,]
    combine_e = rbind(selteam_e, othermatch_e)
  })
  
  PTC_match_f = reactive({
    allmatch_f = PTCallFreq()[PTC_KNNmatch_f(),]
    selteam_f = allmatch_f[1,]
    othermatch_f = allmatch_f[-1,]
    combine_f = rbind(selteam_f, othermatch_f)
  })

  PTC_match_p = reactive({
    allmatch_p = PTCallPerc()[PTC_KNNmatch_p(),]
    selteam_p = allmatch_p[1,]
    othermatch_p = allmatch_p[-1,]
    combine_p = rbind(selteam_p, othermatch_p)
  })

  PTC_PPPdata = reactive({
    if (input$PTC_offdef == "offense") {
      
      PTC_match_e() %>%
        select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
      
    } else{
      
      PTC_match_e() %>%
        select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    }
  })
  
  PTC_FREQdata = reactive({
    if (input$PTC_offdef == "offense") {
      
      PTC_match_f() %>%
        select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
      
    } else{
      
      PTC_match_f() %>%
        select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    }
  })
  
  PTC_PERCdata = reactive({
    if (input$PTC_offdef == "offense") {
      
      PTC_match_p() %>%
        select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
      
    } else{
      
      PTC_match_p() %>%
        select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    }
  })
  
  
  ### Graph
  # Eff Plot
  output$PTC_PPPplot = renderPlotly({
    validate(
      need(dim(PTCothereff())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )
    
    #Assigning Team Names for Colors
    PTC_t1e = as.character(PTC_match_e()[1,1])
    PTC_t2e = as.character(PTC_match_e()[2,1])
    PTC_t3e = as.character(PTC_match_e()[3,1])
    PTC_t4e = as.character(PTC_match_e()[4,1])
    PTC_t5e = as.character(PTC_match_e()[5,1])
    PTC_t6e = as.character(PTC_match_e()[6,1])
    
    
    if (input$PTC_offdef == "offense") {
      
      PTC_effplot = plot_ly(type = "scatterpolar",
                            mode = "closest",
                            fill = "toself",
                            colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[1,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_e()[1,2],
          marker = list(color = color_map[PTC_t1e]),
          fillcolor = toRGB(color_map[PTC_t1e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[2,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[2,2],
          marker = list(color = color_map[PTC_t2e]),
          fillcolor = toRGB(color_map[PTC_t2e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[3,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[3,2],
          marker = list(color = color_map[PTC_t3e]),
          fillcolor = toRGB(color_map[PTC_t3e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[4,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[4,2],
          marker = list(color = color_map[PTC_t4e]),
          fillcolor = toRGB(color_map[PTC_t4e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[5,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[5,2],
          marker = list(color = color_map[PTC_t5e]),
          fillcolor = toRGB(color_map[PTC_t5e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[6,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[6,2],
          marker = list(color = color_map[PTC_t6e]),
          fillcolor = toRGB(color_map[PTC_t6e], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1.5),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_effplot
      
    } else {
      PTC_effplot = plot_ly(type = "scatterpolar",
                            mode = "closest",
                            fill = "toself",
                            colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[1,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_e()[1,2],
          marker = list(color = color_map[PTC_t1e]),
          fillcolor = toRGB(color_map[PTC_t1e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[2,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[2,2],
          marker = list(color = color_map[PTC_t2e]),
          fillcolor = toRGB(color_map[PTC_t2e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[3,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[3,2],
          marker = list(color = color_map[PTC_t3e]),
          fillcolor = toRGB(color_map[PTC_t3e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[4,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[4,2],
          marker = list(color = color_map[PTC_t4e]),
          fillcolor = toRGB(color_map[PTC_t4e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[5,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[5,2],
          marker = list(color = color_map[PTC_t5e]),
          fillcolor = toRGB(color_map[PTC_t5e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PPPdata()[6,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_e()[6,2],
          marker = list(color = color_map[PTC_t6e]),
          fillcolor = toRGB(color_map[PTC_t6e], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1.5),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_effplot
    }
  })
  

  # Eff Table
  output$PTC_PPPtable = renderReactable({
    if (input$PTC_offdef == "offense") {
      
      reactable(PTC_match_e() %>%
                  select(TeamCode, Conf, Div, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 60),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
      
    } else {
      
      reactable(PTC_match_e() %>%
                  select(TeamCode, Conf, Div, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 60),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
    }
  })

  # Freq Plot
  output$PTC_FREQplot = renderPlotly({
    validate(
      need(dim(PTCotherfreq())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )
    
    #Assigning Team Names for Colors
    PTC_t1f = as.character(PTC_match_f()[1,1])
    PTC_t2f = as.character(PTC_match_f()[2,1])
    PTC_t3f = as.character(PTC_match_f()[3,1])
    PTC_t4f = as.character(PTC_match_f()[4,1])
    PTC_t5f = as.character(PTC_match_f()[5,1])
    PTC_t6f = as.character(PTC_match_f()[6,1])
    
    
    if (input$PTC_offdef == "offense") {
      
      PTC_freqplot = plot_ly(type = "scatterpolar",
                            mode = "closest",
                            fill = "toself",
                            colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[1,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_f()[1,2],
          marker = list(color = color_map[PTC_t1f]),
          fillcolor = toRGB(color_map[PTC_t1f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[2,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[2,2],
          marker = list(color = color_map[PTC_t2f]),
          fillcolor = toRGB(color_map[PTC_t2f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[3,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[3,2],
          marker = list(color = color_map[PTC_t3f]),
          fillcolor = toRGB(color_map[PTC_t3f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[4,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[4,2],
          marker = list(color = color_map[PTC_t4f]),
          fillcolor = toRGB(color_map[PTC_t4f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[5,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[5,2],
          marker = list(color = color_map[PTC_t5f]),
          fillcolor = toRGB(color_map[PTC_t5f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[6,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[6,2],
          marker = list(color = color_map[PTC_t6f]),
          fillcolor = toRGB(color_map[PTC_t6f], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,35),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_freqplot
      
    } else {
      PTC_freqplot = plot_ly(type = "scatterpolar",
                            mode = "closest",
                            fill = "toself",
                            colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[1,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_f()[1,2],
          marker = list(color = color_map[PTC_t1f]),
          fillcolor = toRGB(color_map[PTC_t1f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[2,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[2,2],
          marker = list(color = color_map[PTC_t2f]),
          fillcolor = toRGB(color_map[PTC_t2f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[3,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[3,2],
          marker = list(color = color_map[PTC_t3f]),
          fillcolor = toRGB(color_map[PTC_t3f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[4,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[4,2],
          marker = list(color = color_map[PTC_t4f]),
          fillcolor = toRGB(color_map[PTC_t4f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[5,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[5,2],
          marker = list(color = color_map[PTC_t5f]),
          fillcolor = toRGB(color_map[PTC_t5f], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_FREQdata()[6,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_f()[6,2],
          marker = list(color = color_map[PTC_t6f]),
          fillcolor = toRGB(color_map[PTC_t6f], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,35),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_freqplot
    }
  })
  
  # Freq Table
  output$PTC_FREQtable = renderReactable({
    if (input$PTC_offdef == "offense") {
      
      reactable(PTC_match_f() %>%
                  select(TeamCode, Conf, Div, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 90),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
      
    } else {
      
      reactable(PTC_match_f() %>%
                  select(TeamCode, Conf, Div, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 90),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
    }
  })
  
  
  # Perc Plot
  output$PTC_PERCplot = renderPlotly({
    validate(
      need(dim(PTCotherperc())[1]>=5, "Sorry, the required number of matches was not met. Please change the input filters.")
    )
    
    PTC_t1p = as.character(PTC_match_p()[1,1])
    PTC_t2p = as.character(PTC_match_p()[2,1])
    PTC_t3p = as.character(PTC_match_p()[3,1])
    PTC_t4p = as.character(PTC_match_p()[4,1])
    PTC_t5p = as.character(PTC_match_p()[5,1])
    PTC_t6p = as.character(PTC_match_p()[6,1])

    
    if (input$PTC_offdef == "offense") {
      
      PTC_percplot = plot_ly(type = "scatterpolar",
                             mode = "closest",
                             fill = "toself",
                             colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[1,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_p()[1,2],
          marker = list(color = color_map[PTC_t1p]),
          fillcolor = toRGB(color_map[PTC_t1p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[2,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[2,2],
          marker = list(color = color_map[PTC_t2p]),
          fillcolor = toRGB(color_map[PTC_t2p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[3,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[3,2],
          marker = list(color = color_map[PTC_t3p]),
          fillcolor = toRGB(color_map[PTC_t3p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[4,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[4,2],
          marker = list(color = color_map[PTC_t4p]),
          fillcolor = toRGB(color_map[PTC_t4p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[5,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[5,2],
          marker = list(color = color_map[PTC_t5p]),
          fillcolor = toRGB(color_map[PTC_t5p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[6,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[6,2],
          marker = list(color = color_map[PTC_t6p]),
          fillcolor = toRGB(color_map[PTC_t6p], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,100),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_percplot
      
    } else {
      
      PTC_percplot = plot_ly(type = "scatterpolar",
                             mode = "closest",
                             fill = "toself",
                             colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[1,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = PTC_match_p()[1,2],
          marker = list(color = color_map[PTC_t1p]),
          fillcolor = toRGB(color_map[PTC_t1p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[2,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[2,2],
          marker = list(color = color_map[PTC_t2p]),
          fillcolor = toRGB(color_map[PTC_t2p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[3,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[3,2],
          marker = list(color = color_map[PTC_t3p]),
          fillcolor = toRGB(color_map[PTC_t3p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[4,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[4,2],
          marker = list(color = color_map[PTC_t4p]),
          fillcolor = toRGB(color_map[PTC_t4p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[5,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[5,2],
          marker = list(color = color_map[PTC_t5p]),
          fillcolor = toRGB(color_map[PTC_t5p], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(PTC_PERCdata()[6,])),
          theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = PTC_match_p()[6,2],
          marker = list(color = color_map[PTC_t6p]),
          fillcolor = toRGB(color_map[PTC_t6p], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,100),
              tickfont = list(size = 11)),
            angularaxis = list(tickfont = list(size = 11))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      PTC_percplot
      
    }
  })

  # Perc Table
  output$PTC_PERCtable = renderReactable({
    if (input$PTC_offdef == "offense") {
      
      reactable(PTC_match_p() %>%
                  select(TeamCode, Conf, Div, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 90),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
      
    } else {
      
      reactable(PTC_match_p() %>%
                  select(TeamCode, Conf, Div, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition),
                pagination = FALSE, striped = FALSE, searchable = FALSE,
                selection = "single", onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
                ),
                defaultColDef = colDef(align = "center",
                                       minWidth = 90),
                columns = list(
                  TeamCode = colDef(name = "Team"),
                  Div = colDef(name = "Division"),
                  Conf = colDef(name = "Conference"),
                  .selection = colDef(show = FALSE)
                ),
                showSortIcon = FALSE,
                highlight = TRUE) 
      
    }
  })

  # Full Tables
  playtypeMatchEff = reactive({
    playtypes %>%
    filter(TeamCode %in% PTC_match_e()$TeamCode) %>%
      filter((Team != input$PTC_team)&(Season != input$PTC_season))
  })
  
  output$PTC_PPPtable2 = renderReactable({
    reactable(playtypeMatchEff() %>%
                select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,20,30,50), 
              striped = FALSE, searchable = FALSE, defaultSorted = "PlayType", defaultSortOrder = "asc", filterable = TRUE,
              selection = "single", onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
              ),
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                TeamCode = colDef(name = "Team"),
                Div = colDef(name = "Division"),
                Conf = colDef(name = "Conference"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct"),
                .selection = colDef(show = FALSE)
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  playtypeMatchFreq = reactive({
    playtypes %>%
      filter(TeamCode %in% PTC_match_f()$TeamCode) %>%
      filter((Team != input$PTC_team)&(Season != input$PTC_season))
  })
  
  output$PTC_FREQtable2 = renderReactable({
    reactable(playtypeMatchFreq() %>%
                select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,20,30,50), striped = FALSE, searchable = FALSE, defaultSorted = "PlayType", defaultSortOrder = "asc", filterable = TRUE,
              selection = "single", onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
              ),
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                TeamCode = colDef(name = "Team"),
                Div = colDef(name = "Division"),
                Conf = colDef(name = "Conference"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct"),
                .selection = colDef(show = FALSE)
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
                select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary),
              pagination = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,20,30,50),
              striped = FALSE, searchable = FALSE, defaultSorted = "PlayType", defaultSortOrder = "asc", filterable = TRUE,
              selection = "single", onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
              ),
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                TeamCode = colDef(name = "Team"),
                Div = colDef(name = "Division"),
                Conf = colDef(name = "Conference"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Pct"),
                .selection = colDef(show = FALSE)
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  output$PTC_PPPtabledownload2 <- downloadHandler(
    filename = function() {
      paste0(as.character(input$PTC_team),"_",as.character(input$PTC_season),"_",as.character(input$PTC_offdef),'efficiencymatchtable', '.csv')
    },
    content = function(file) {
      PPPtabledown1 = playtypeMatchEff()
      
      PPPtabledown2 <- PPPtabledown1 %>% 
        select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary)  
      
      write.csv(PPPtabledown2, file)
    }
  )
  
  output$PTC_FREQtabledownload2 <- downloadHandler(
    filename = function() {
      paste0(as.character(input$PTC_team),"_",as.character(input$PTC_season),"_",as.character(input$PTC_offdef),'frequencymatchtable', '.csv')
    },
    content = function(file) {
      FREQtabledown1 = playtypeMatchFreq()
      
      FREQtabledown2 <- FREQtabledown1 %>% 
        select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary)  
      
      write.csv(FREQtabledown2, file)
    }
  )
  
  output$PTC_PERCtabledownload2 <- downloadHandler(
    filename = function() {
      paste0(as.character(input$PTC_team),"_",as.character(input$PTC_season),"_",as.character(input$PTC_offdef),'percentilematchtable', '.csv')
    },
    content = function(file) {
      PERCtabledown1 = playtypeMatchPerc()
      
      PERCtabledown2 <- PERCtabledown1 %>% 
        select(-Team, -SeasonRange, -Conf, -Div, -Season, -GP, -Mins, -Playoff, -name, -primary, -secondary)  
      
      write.csv(PERCtabledown2, file)
    }
  )
  
  
  observeEvent(input$PTC_reset,{
    updateSelectInput(session, 'PTC_team', selected = "MIL")
    updateSelectInput(session, 'PTC_season', selected = "2019-2020")
    updateCheckboxGroupInput(session, 'PTC_conf', selected = c("West" = "West", "East" = "East"))
    updateRadioButtons(session, 'PTC_offdef', selected = "offense")
  })
  
  
  ############## SERVER CODE FOR 'MULTIPLE TEAM PLAYTYPE COMPARISONS' TAB ################
  # Getting Tables
  MTC_teams_e = reactive({
    playtypeEff %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Order = row_number())
  })
  MTC_teams_f = reactive({
    playtypeFreq %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Order = row_number())
  })
  MTC_teams_p = reactive({
    playtypePerc %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Order = row_number())
  })
  
  # Eff Plot - Offense
  output$MTC_OffEffPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_e())[1]>=1, "")
    )
    
    MTC_oe_data = MTC_teams_e() %>%
      filter(OffDef == "offense") %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_oe_fulldata = MTC_teams_e() %>%
      filter(OffDef == "offense") %>%
      select(Team, TeamCode, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1e = as.character(MTC_oe_fulldata[1,1])
    MTC_t2e = as.character(MTC_oe_fulldata[2,1])
    MTC_t3e = as.character(MTC_oe_fulldata[3,1])
    MTC_t4e = as.character(MTC_oe_fulldata[4,1])
    MTC_t5e = as.character(MTC_oe_fulldata[5,1])
   
      MTC_oe_plot = plot_ly(type = "scatterpolar",
                            mode = "markers",
                            fill = "toself",
                            colors = color_map) %>%
        add_trace(
          r = as.numeric(as.matrix(MTC_oe_data[1,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          name = MTC_oe_fulldata[1,2],
          marker = list(color = color_map[MTC_t1e]),
          fillcolor = toRGB(color_map[MTC_t1e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(MTC_oe_data[2,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = MTC_oe_fulldata[2,2],
          marker = list(color = color_map[MTC_t2e]),
          fillcolor = toRGB(color_map[MTC_t2e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(MTC_oe_data[3,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = MTC_oe_fulldata[3,2],
          marker = list(color = color_map[MTC_t3e]),
          fillcolor = toRGB(color_map[MTC_t3e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(MTC_oe_data[4,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = MTC_oe_fulldata[4,2],
          marker = list(color = color_map[MTC_t4e]),
          fillcolor = toRGB(color_map[MTC_t4e], alpha = 0.5)
        ) %>%
        add_trace(
          r = as.numeric(as.matrix(MTC_oe_data[5,])),
          theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
          showlegend = TRUE,
          mode = "markers",
          visible = "legendonly",
          name = MTC_oe_fulldata[5,2],
          marker = list(color = color_map[MTC_t5e]),
          fillcolor = toRGB(color_map[MTC_t5e], alpha = 0.5)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1.5),
              tickfont = list(size = 9)),
            angularaxis = list(tickfont = list(size = 8))),
          showlegend = TRUE,
          legend = list(font = list(size = 10)))
      MTC_oe_plot
  })
  
  # Eff Plot - Defense
  output$MTC_DefEffPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_e())[1]>=1, "")
    )
    
    MTC_de_data = MTC_teams_e() %>%
      filter(OffDef == "defense") %>%
      select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_de_fulldata = MTC_teams_e() %>%
      filter(OffDef == "defense") %>%
      select(Team, TeamCode, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1de = as.character(MTC_de_fulldata[1,1])
    MTC_t2de = as.character(MTC_de_fulldata[2,1])
    MTC_t3de = as.character(MTC_de_fulldata[3,1])
    MTC_t4de = as.character(MTC_de_fulldata[4,1])
    MTC_t5de = as.character(MTC_de_fulldata[5,1])
    
    MTC_de_plot = plot_ly(type = "scatterpolar",
                          mode = "markers",
                          fill = "toself",
                          colors = color_map) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_de_data[1,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = MTC_de_fulldata[1,2],
        marker = list(color = color_map[MTC_t1de]),
        fillcolor = toRGB(color_map[MTC_t1de], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_de_data[2,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_de_fulldata[2,2],
        marker = list(color = color_map[MTC_t2de]),
        fillcolor = toRGB(color_map[MTC_t2de], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_de_data[3,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_de_fulldata[3,2],
        marker = list(color = color_map[MTC_t3de]),
        fillcolor = toRGB(color_map[MTC_t3de], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_de_data[4,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_de_fulldata[4,2],
        marker = list(color = color_map[MTC_t4de]),
        fillcolor = toRGB(color_map[MTC_t4de], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_de_data[5,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_de_fulldata[5,2],
        marker = list(color = color_map[MTC_t5de]),
        fillcolor = toRGB(color_map[MTC_t5de], alpha = 0.5)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1.5),
            tickfont = list(size = 9)),
          angularaxis = list(tickfont = list(size = 8))),
        showlegend = TRUE,
        legend = list(font = list(size = 10)))
    MTC_de_plot
  })
  
  # Freq Plot - Offense
  output$MTC_OffFreqPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_f())[1]>=1, "Choose at least 1 team to display graphs.")
    )
    
    MTC_of_data = MTC_teams_f() %>%
      filter(OffDef == "offense") %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_of_fulldata = MTC_teams_f() %>%
      filter(OffDef == "offense") %>%
      select(Team, TeamCode, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1f = as.character(MTC_of_fulldata[1,1])
    MTC_t2f = as.character(MTC_of_fulldata[2,1])
    MTC_t3f = as.character(MTC_of_fulldata[3,1])
    MTC_t4f = as.character(MTC_of_fulldata[4,1])
    MTC_t5f = as.character(MTC_of_fulldata[5,1])
    
    MTC_of_plot = plot_ly(type = "scatterpolar",
                          mode = "markers",
                          fill = "toself",
                          colors = color_map) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_of_data[1,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = MTC_of_fulldata[1,2],
        marker = list(color = color_map[MTC_t1f]),
        fillcolor = toRGB(color_map[MTC_t1f], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_of_data[2,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_of_fulldata[2,2],
        marker = list(color = color_map[MTC_t2f]),
        fillcolor = toRGB(color_map[MTC_t2f], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_of_data[3,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_of_fulldata[3,2],
        marker = list(color = color_map[MTC_t3f]),
        fillcolor = toRGB(color_map[MTC_t3f], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_of_data[4,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_of_fulldata[4,2],
        marker = list(color = color_map[MTC_t4f]),
        fillcolor = toRGB(color_map[MTC_t4f], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_of_data[5,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_of_fulldata[5,2],
        marker = list(color = color_map[MTC_t5f]),
        fillcolor = toRGB(color_map[MTC_t5f], alpha = 0.5)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,35),
            tickfont = list(size = 9)),
          angularaxis = list(tickfont = list(size = 8))),
        showlegend = TRUE,
        legend = list(font = list(size = 10)))
    MTC_of_plot
  })
  
  # Freq Plot - Defense
  output$MTC_DefFreqPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_f())[1]>=1, "")
    )
    
    MTC_df_data = MTC_teams_f() %>%
      filter(OffDef == "defense") %>%
      select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_df_fulldata = MTC_teams_f() %>%
      filter(OffDef == "defense") %>%
      select(Team, TeamCode, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1df = as.character(MTC_df_fulldata[1,1])
    MTC_t2df = as.character(MTC_df_fulldata[2,1])
    MTC_t3df = as.character(MTC_df_fulldata[3,1])
    MTC_t4df = as.character(MTC_df_fulldata[4,1])
    MTC_t5df = as.character(MTC_df_fulldata[5,1])
    
    MTC_df_plot = plot_ly(type = "scatterpolar",
                          mode = "markers",
                          fill = "toself",
                          colors = color_map) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_df_data[1,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = MTC_df_fulldata[1,2],
        marker = list(color = color_map[MTC_t1df]),
        fillcolor = toRGB(color_map[MTC_t1df], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_df_data[2,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_df_fulldata[2,2],
        marker = list(color = color_map[MTC_t2df]),
        fillcolor = toRGB(color_map[MTC_t2df], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_df_data[3,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_df_fulldata[3,2],
        marker = list(color = color_map[MTC_t3df]),
        fillcolor = toRGB(color_map[MTC_t3df], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_df_data[4,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_df_fulldata[4,2],
        marker = list(color = color_map[MTC_t4df]),
        fillcolor = toRGB(color_map[MTC_t4df], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_df_data[5,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_df_fulldata[5,2],
        marker = list(color = color_map[MTC_t5df]),
        fillcolor = toRGB(color_map[MTC_t5df], alpha = 0.5)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,35),
            tickfont = list(size = 9)),
          angularaxis = list(tickfont = list(size = 8))),
        showlegend = TRUE,
        legend = list(font = list(size = 10)))
    MTC_df_plot
  })
  
  # Perc Plot - Offense
  output$MTC_OffPercPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_p())[1]>=1, "")
    )
    
    MTC_op_data = MTC_teams_p() %>%
      filter(OffDef == "offense") %>%
      select(Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_op_fulldata = MTC_teams_p() %>%
      filter(OffDef == "offense") %>%
      select(Team, TeamCode, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1p = as.character(MTC_op_fulldata[1,1])
    MTC_t2p = as.character(MTC_op_fulldata[2,1])
    MTC_t3p = as.character(MTC_op_fulldata[3,1])
    MTC_t4p = as.character(MTC_op_fulldata[4,1])
    MTC_t5p = as.character(MTC_op_fulldata[5,1])
    
    MTC_op_plot = plot_ly(type = "scatterpolar",
                          mode = "markers",
                          fill = "toself",
                          colors = color_map) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_op_data[1,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = MTC_op_fulldata[1,2],
        marker = list(color = color_map[MTC_t1p]),
        fillcolor = toRGB(color_map[MTC_t1p], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_op_data[2,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_op_fulldata[2,2],
        marker = list(color = color_map[MTC_t2p]),
        fillcolor = toRGB(color_map[MTC_t2p], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_op_data[3,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_op_fulldata[3,2],
        marker = list(color = color_map[MTC_t3p]),
        fillcolor = toRGB(color_map[MTC_t3p], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_op_data[4,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_op_fulldata[4,2],
        marker = list(color = color_map[MTC_t4p]),
        fillcolor = toRGB(color_map[MTC_t4p], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_op_data[5,])),
        theta = c("Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_op_fulldata[5,2],
        marker = list(color = color_map[MTC_t5p]),
        fillcolor = toRGB(color_map[MTC_t5p], alpha = 0.5)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100),
            tickfont = list(size = 9)),
          angularaxis = list(tickfont = list(size = 8))),
        showlegend = TRUE,
        legend = list(font = list(size = 10)))
    MTC_op_plot
  })
  
  # Freq Plot - Defense
  output$MTC_DefPercPlot = renderPlotly({
    
    validate(
      need(dim(MTC_teams_p())[1]>=1, "")
    )
    
    MTC_dp_data = MTC_teams_p() %>%
      filter(OffDef == "defense") %>%
      select(Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    MTC_dp_fulldata = MTC_teams_p() %>%
      filter(OffDef == "defense") %>%
      select(Team, TeamCode, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    #Assigning Team Names for Colors
    MTC_t1dp = as.character(MTC_dp_fulldata[1,1])
    MTC_t2dp = as.character(MTC_dp_fulldata[2,1])
    MTC_t3dp = as.character(MTC_dp_fulldata[3,1])
    MTC_t4dp = as.character(MTC_dp_fulldata[4,1])
    MTC_t5dp = as.character(MTC_dp_fulldata[5,1])
    
    MTC_dp_plot = plot_ly(type = "scatterpolar",
                          mode = "markers",
                          fill = "toself",
                          colors = color_map) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_dp_data[1,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        name = MTC_dp_fulldata[1,2],
        marker = list(color = color_map[MTC_t1dp]),
        fillcolor = toRGB(color_map[MTC_t1dp], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_dp_data[2,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_dp_fulldata[2,2],
        marker = list(color = color_map[MTC_t2dp]),
        fillcolor = toRGB(color_map[MTC_t2dp], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_dp_data[3,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_dp_fulldata[3,2],
        marker = list(color = color_map[MTC_t3dp]),
        fillcolor = toRGB(color_map[MTC_t3dp], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_dp_data[4,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_dp_fulldata[4,2],
        marker = list(color = color_map[MTC_t4dp]),
        fillcolor = toRGB(color_map[MTC_t4dp], alpha = 0.5)
      ) %>%
      add_trace(
        r = as.numeric(as.matrix(MTC_dp_data[5,])),
        theta = c("Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks", "SpotUp", "Transition"),
        showlegend = TRUE,
        mode = "markers",
        visible = "legendonly",
        name = MTC_dp_fulldata[5,2],
        marker = list(color = color_map[MTC_t5dp]),
        fillcolor = toRGB(color_map[MTC_t5dp], alpha = 0.5)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100),
            tickfont = list(size = 9)),
          angularaxis = list(tickfont = list(size = 8))),
        showlegend = TRUE,
        legend = list(font = list(size = 10)))
    MTC_dp_plot
  })
  
  # MTC Sum Table
  output$MTC_SumTable = renderReactable({
    validate(
      need(length(input$MTC_teams)>=1, "Select more than 1 team to display summary table")
    )
    
    tabEff = playtypeEff %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Metric = "PPP/Efficiency") %>%
      select(TeamCode, Conf, Div, Metric, OffDef, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    tabFreq = playtypeFreq %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Metric = "Frequency (%)") %>%
      select(TeamCode, Conf, Div, Metric, OffDef, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    tabPerc = playtypePerc %>%
      filter(TeamCode %in% input$MTC_teams) %>%
      mutate(Metric = "Percentile") %>%
      select(TeamCode, Conf, Div, Metric, OffDef, Cut, Handoff, Iso, OffScreen, PNRHandler, PNRRollman, PostUp, Putbacks, SpotUp, Transition)
    
    tabFull = rbind(tabEff, tabFreq, tabPerc) %>%
      arrange(TeamCode, Metric)
    
    reactable(tabFull,
              pagination = FALSE, striped = FALSE, searchable = FALSE, filterable = TRUE, 
              selection = "single", onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23, 64, 139, 0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 2px 0 0 0 #C9082A")
              ),
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                TeamCode = colDef(name = "Team"),
                Div = colDef(name = "Division"),
                Conf = colDef(name = "Conference"),
                .selection = colDef(show = FALSE)
              ),
              showSortIcon = FALSE,
              highlight = TRUE)
  })
  
  
  observeEvent(input$MTC_reset,{
    updateSelectizeInput(session, "MTC_teams", selected = "")
  })
  
  
  ############## SERVER CODE FOR '5-YEAR WINDOW ANALYSIS' TAB ################
  WA_maintab = reactive({
    genteams %>%
      select(Team, SeasonRange, Conf, Div, Wins, Losses, WinPerc, EstWinPerc, PPG, OppPPG, AvgPTDiff, Pace, oEFF, dEFF, EFFDiff, 
             OffRtg, DefRtg, NetRtg, eFGPercSeason, TSPerc, ASTPerc, ASTtoTOV,ORebPerc, RebPerc, TOVPerc, primary)
  })
  
  output$WA_Table = renderReactable({
    reactable(WA_maintab(),
              sortable = TRUE, filterable = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,20,30,50),
              paginationType = "numbers", selection = "single", onClick = "select", selectionId = "WA_tabselect", striped = FALSE, showSortIcon = FALSE, highlight = TRUE,
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "rgba(23,64,139,0.9)", color = "#FFF", fontWeight = "600", boxShadow = "inset 4px 0 0 0 #C9082A")
              ),
              defaultColDef = colDef(align = "center",
                                     minWidth = 90),
              columns = list(
                SeasonRange = colDef(name = "Season"),
                Conf = colDef(name = "Conference"),
                Div = colDef(name = "Division"),
                Wins = colDef(name = "W"),
                Losses = colDef(name = "L"),
                WinPerc = colDef(name = "Win%"),
                EstWinPerc = colDef(name = "estWin%"),
                AvgPTDiff = colDef(name = "PPGDiff"),
                eFGPercSeason = colDef(name = "eFG%"),
                TSPerc = colDef(name = "TS%"),
                ASTPerc = colDef(name = "AST%"),
                ASTtoTOV = colDef(name = "AST/TO"),
                ORebPerc = colDef(name = "OReb%"),
                RebPerc = colDef(name = "Reb%"),
                TOVPerc = colDef(name = "TOV%"),
                primary = colDef(show = FALSE),
                .selection = colDef(show = FALSE)
                ))
  })
  
  selected = reactive({
    getReactableState("WA_Table", "selected")
  })
  
  ### Plot Output 1
  output$WA_titleappear1 = renderUI({
    if(length(selected())>0){
      textOutput("WA_title1")
    }
  })
  
  output$WA_subtitleappear1 = renderUI({
    if(length(selected())>0){
      textOutput("WA_subtitle1")
    }
  })
  
  output$WA_plotappear1 = renderUI({
    if(length(selected())>0){
      plotOutput("WA_plot1")
    }
  })
  
  output$WA_title1 = renderText({
    paste0(genteamsWA_A[selected(),]$Team, " (", genteamsWA_A[selected(),]$SeasonRange,")")
  })
  
  output$WA_subtitle1 = renderText({
    paste0("Amount Above Season Average")
  })
  
  output$WA_plot1 = renderPlot({
    A1set = genteamsWA_A %>%
      filter(TeamCode == genteamsWA_A[selected(),]$TeamCode) %>%
      pivot_longer(cols = c(5,7,9,11,13,15,17,19,21), names_to = "Metrics", values_to = "AmountAboveSeasonAverage") %>%
      select(1:4,tail(names(.),2)) %>%
      mutate(posneg = ifelse(AmountAboveSeasonAverage < 0, "neg", "pos"))
    
    ggplot(A1set, aes(x = reorder(Metrics, AmountAboveSeasonAverage), y = AmountAboveSeasonAverage, fill = posneg)) +
      geom_bar(stat = "identity", show.legend = F, width = 0.6) +
      geom_hline(yintercept = 0, color = "#E3BC2C", size = 1.5) +
      scale_fill_manual(values = c(neg = "#AA1A1A", pos = "#31A217")) +
      scale_x_discrete(labels=c("PPGDiff1" = "Points per Game",
                                "PaceDiff1" = "Pace",
                                "OppPPGDiff1" = "Opp Points per Game",
                                "OffRtgDiff1" = "Offensive Rating",
                                "oEFFDiff1" = "Offensive Efficiency",
                                "NetRtgDiff1" = "Net Rating",
                                "EFFDiffDiff1" = "Net Efficiency",
                                "DefRtgDiff1" = "Defensive Rating",
                                "dEFFDiff1" = "Defensive Efficiency")) +
      scale_y_continuous(breaks = seq(floor(min(A1set$AmountAboveSeasonAverage)), ceiling(max(A1set$AmountAboveSeasonAverage)), by = 2)) +
      coord_flip() + 
      labs(x = "",
           y = "",
           caption = "Brett Kornfeld   |   Gabby Herrera-Lim   |   Source: NBAstuffer") +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(colour = "#E4E4E4"),
            panel.grid.minor = element_line(color = "#E4E4E4"),
            panel.background = element_rect(fill = 'white'),
            axis.ticks = element_blank(),
            axis.text.y = element_text(face = "bold"))
  })
  
  ### Plot Output 2
  output$WA_subtitleappear2 = renderUI({
    if(length(selected())>0){
      textOutput("WA_subtitle2")
    }
  })
  
  output$WA_plotappear2 = renderUI({
    if(length(selected())>0){
      plotOutput("WA_plot2")
    }
  })
  
  output$WA_subtitle2 = renderText({
    paste0("Percent Above Season Average")
  })
  
    output$WA_plot2 = renderPlot({
    A2set = genteamsWA_P %>%
      filter(TeamCode == genteamsWA_P[selected(),]$TeamCode) %>%
      pivot_longer(cols = c(5,7,9,11,13,15,17,19,21), names_to = "Metrics", values_to = "PercAboveSeasonAverage") %>%
      select(1:4,tail(names(.),2)) %>%
      mutate(posneg = ifelse(PercAboveSeasonAverage < 0, "neg", "pos"))
    
    ggplot(A2set, aes(x = reorder(Metrics, PercAboveSeasonAverage), y = PercAboveSeasonAverage, fill = posneg)) +
      geom_bar(stat = "identity", show.legend = F, width = 0.6) +
      geom_hline(yintercept = 0, color = "#E3BC2C", size = 1.5) +
      scale_fill_manual(values = c(neg = "#AA1A1A", pos = "#31A217")) +
      scale_x_discrete(labels=c("WPDiff1" = "Win Percentage",
                                "EWPDiff1" = "Est. Win Percentage",
                                "EFGDiff1" = "Effective FG%",
                                "TSDiff1" = "True Shooting %",
                                "ASTDiff1" = "Assist %",
                                "TODiff1" = "Turnover %",
                                "ATDiff1" = "Assist-to-Turnover Ratio",
                                "OREBDiff1" = "Off. Rebound %",
                                "REBDiff1" = "Rebound %")) +
      scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                         breaks = seq(floor(min(A2set$PercAboveSeasonAverage)), ceiling(max(A2set$PercAboveSeasonAverage)), by = 0.1)) +
      coord_flip() + 
      labs(x = "",
           y = "",
           caption = "Brett Kornfeld   |   Gabby Herrera-Lim   |   Source: NBAstuffer") +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(colour = "#E4E4E4"),
            panel.grid.minor = element_line(color = "#E4E4E4"),
            panel.background = element_rect(fill = 'white'),
            axis.ticks = element_blank(),
            axis.text.y = element_text(face = "bold"))
  })
  
  ### Plot Output 3
  output$WA_subtitleappear3 = renderUI({
    if(length(selected())>0){
      textOutput("WA_subtitle3")
    }
  })
  
  output$WA_plotappear3 = renderUI({
    if(length(selected())>0){
      plotOutput("WA_plot3")
    }
  })
  
  output$WA_subtitle3 = renderText({
    paste0("Amount Above 5-Year Average")
  })
  
  output$WA_plot3 = renderPlot({
    A3set = genteamsWA_A %>%
      filter(TeamCode == genteamsWA_A[selected(),]$TeamCode) %>%
      pivot_longer(cols = c(6,8,10,12,14,16,18,20,22), names_to = "Metrics", values_to = "AmountAboveWindowAverage") %>%
      select(1:4,tail(names(.),2)) %>%
      mutate(posneg = ifelse(AmountAboveWindowAverage < 0, "neg", "pos"))
    
    ggplot(A3set, aes(x = reorder(Metrics, AmountAboveWindowAverage), y = AmountAboveWindowAverage, fill = posneg)) +
      geom_bar(stat = "identity", show.legend = F, width = 0.6) +
      geom_hline(yintercept = 0, color = "#E3BC2C", size = 1.5) +
      scale_fill_manual(values = c(neg = "#AA1A1A", pos = "#31A217")) +
      scale_x_discrete(labels=c("PPGDiff5" = "Points per Game",
                                "PaceDiff5" = "Pace",
                                "OppPPGDiff5" = "Opp Points per Game",
                                "OffRtgDiff5" = "Offensive Rating",
                                "oEFFDiff5" = "Offensive Efficiency",
                                "NetRtgDiff5" = "Net Rating",
                                "EFFDiffDiff5" = "Net Efficiency",
                                "DefRtgDiff5" = "Defensive Rating",
                                "dEFFDiff5" = "Defensive Efficiency")) +
      scale_y_continuous(breaks = seq(floor(min(A3set$AmountAboveWindowAverage)), ceiling(max(A3set$AmountAboveWindowAverage)), by = 2)) +
      coord_flip() + 
      labs(x = "",
           y = "",
           caption = "Brett Kornfeld   |   Gabby Herrera-Lim   |   Source: NBAstuffer") +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(colour = "#E4E4E4"),
            panel.grid.minor = element_line(color = "#E4E4E4"),
            panel.background = element_rect(fill = 'white'),
            axis.ticks = element_blank(),
            axis.text.y = element_text(face = "bold"))
  })
  
  ### Plot Output 4
  output$WA_subtitleappear4 = renderUI({
    if(length(selected())>0){
      textOutput("WA_subtitle4")
    }
  })
  
  output$WA_plotappear4 = renderUI({
    if(length(selected())>0){
      plotOutput("WA_plot4")
    }
  })
  
  output$WA_subtitle4 = renderText({
    paste0("Percent Above 5-Year Average")
  })
  
  output$WA_plot4 = renderPlot({
    A4set = genteamsWA_P %>%
      filter(TeamCode == genteamsWA_P[selected(),]$TeamCode) %>%
      pivot_longer(cols = c(6,8,10,12,14,16,18,20,22), names_to = "Metrics", values_to = "PercentAboveWindowAverage") %>%
      select(1:4,tail(names(.),2)) %>%
      mutate(posneg = ifelse(PercentAboveWindowAverage < 0, "neg", "pos"))
    
    ggplot(A4set, aes(x = reorder(Metrics, PercentAboveWindowAverage), y = PercentAboveWindowAverage, fill = posneg)) +
      geom_bar(stat = "identity", show.legend = F, width = 0.6) +
      geom_hline(yintercept = 0, color = "#E3BC2C", size = 1.5) +
      scale_fill_manual(values = c(neg = "#AA1A1A", pos = "#31A217")) +
      scale_x_discrete(labels=c("WPDiff5" = "Win Percentage",
                                "EWPDiff5" = "Est. Win Percentage",
                                "EFGDiff5" = "Effective FG%",
                                "TSDiff5" = "True Shooting %",
                                "ASTDiff5" = "Assist %",
                                "TODiff5" = "Turnover %",
                                "ATDiff5" = "Assist-to-Turnover Ratio",
                                "OREBDiff5" = "Off. Rebound %",
                                "REBDiff5" = "Rebound %")) +
      scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                         breaks = seq(floor(min(A4set$PercentAboveWindowAverage)), ceiling(max(A4set$PercentAboveWindowAverage)), by = 0.1)) +
      coord_flip() + 
      labs(x = "",
           y = "",
           caption = "Brett Kornfeld   |   Gabby Herrera-Lim   |   Source: NBAstuffer") +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(colour = "#E4E4E4"),
            panel.grid.minor = element_line(color = "#E4E4E4"),
            panel.background = element_rect(fill = 'white'),
            axis.ticks = element_blank(),
            axis.text.y = element_text(face = "bold"))
  })
}