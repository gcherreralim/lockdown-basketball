library(shiny)
library(shinythemes)
library(ggplot2)
library(magrittr)
library(DT)
library(reactable)
library(shinyWidgets)
library(shinysky)
library(tidyverse)
library(httr)
library(stringr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(caret)
library(formattable)
library(data.table)
library(plotly)
library(highcharter)
library(RColorBrewer)
library(htmltools)
library(shinyjs)
library(leaflet)
library(reshape2)
library(class)
library(FNN)

#Loading Data
fullteams = readr::read_csv("fullteams2.csv")

fullteams = fullteams %>%
  mutate(season = season + 1) %>%
  mutate(teamcode = paste0(team,season))

fullteams = fullteams %>%
  dplyr::select(team, teamcode, CONF, DIV, season, YearRange, WINS, LOSSES, WINPerc, EWINPerc, PROJWINPerc, AchievementLevel, MIN, playtype, offdef, games_played, playtypepossessions, 
                playtypefrequency, playtypePPP, playtypepoints, playtypefg_made, playtypefg_perc, playtypeeff_fg_perc, playtypeft_freq, playtypetov_freq, playtypesf_freq, playtypeand1_freq, 
                playtypescore_freq, playtypepercentile, PPG, OPPPPG, AVGPTDIFF, PACE, oEFF, dEFF, EFFDIFF, SOS, SCHEDADJRATING, CONSISTENCYRATING, ADJ4FACTORS, OffRtg, DefRtg, NetRtg, ASTPerc, 
                ASTToTO, ASTRatio, OREBPerc, DREBPerc, REBPerc, TOVPerc, eFGPerc, TSPerc, PIE, Playoff)

colnamesNew = c("Team", "TeamCode", "Conf", "Div", "Season", "SeasonRange", "Wins", "Losses", "WinPerc", "EstWinPerc", "ProjWinPerc", "AchLevel", "Mins", "PlayType", "OffDef", "GP",
                "Poss", "Freq", "PPP", "Points", "FGM", "FGPerc", "EFGPerc", "FTFreq", "TOVFreq", "SFFreq", "And1Freq", "ScoreFreq", "Percentile", "PPG", "OppPPG", "AvgPTDiff", "Pace",
                "oEFF", "dEFF", "EFFDiff", "SOS", "SchedAdjRTG", "Consistency", "Adj4Factors", "OffRtg", "DefRtg", "NetRtg", "ASTPerc", "ASTtoTOV", "ASTRatio", "ORebPerc", "DRebPerc",
                "RebPerc", "TOVPerc", "eFGPercSeason", "TSPerc", "PIE", "Playoff")

colnames(fullteams) = colnamesNew
