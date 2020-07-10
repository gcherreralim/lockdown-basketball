### USED PACKAGES
pack = c("shiny", "ggplot2", "magrittr", "reactable","tidyverse", "httr", "stringr", "lubridate", "plotly",
         "shinydashboard", "caret", "formattable", "data.table", "highcharter",
         "htmltools", "shinyjs", "leaflet", "reshape2", "class", 
         "FNN", "teamcolors", "ggrepel", "extrafont", "showtext","scales")

# VERIFY PACKAGES
# package.check <- lapply(pack, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#   }
# })

library(shiny)
library(ggplot2)
library(magrittr)
library(reactable)
library(tidyverse)
library(httr)
library(stringr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(caret)
library(formattable)
library(data.table)
library(highcharter)
library(htmltools)
library(shinyjs)
library(leaflet)
library(reshape2)
library(class)
library(FNN)
library(teamcolors)
library(ggrepel)
library(extrafont)
library(showtext)
library(scales)


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
#fullteams = fullteams %>%
#  mutate(WinPerc = label_percent(accuracy = 0.01)(WinPerc))
fullteams = fullteams %>%
  mutate(SeasonRange = as.factor(SeasonRange))

playtypes = fullteams[c(1:9,13:29,54)]
genteams = fullteams[c(1:12,30:54)]

genteams = genteams %>%
  distinct()

playtypeFreq = playtypes %>%
  select(Team, TeamCode, Conf, Div, Season, SeasonRange, PlayType, OffDef, Freq) %>%
  pivot_wider(names_from = PlayType, values_from = Freq) %>%
  select(-misc, misc)

playtypeEff = playtypes %>%
  select(Team, TeamCode, Conf, Div, Season, SeasonRange, PlayType, OffDef, PPP) %>%
  pivot_wider(names_from = PlayType, values_from = PPP) %>%
  select(-misc, misc)

playtypePerc = playtypes %>%
  select(Team, TeamCode, Conf, Div, Season, SeasonRange, PlayType, OffDef, Percentile) %>%
  pivot_wider(names_from = PlayType, values_from = Percentile) %>%
  select(-misc, misc)

colnamesPT = c("Team", "TeamCode", "Conf", "Div", "Season", "SeasonRange", "OffDef", "Cut", "Handoff", "Iso", "OffScreen", "PNRHandler", "PNRRollman", "PostUp", "Putbacks","SpotUp", "Transition", "Misc")
colnames(playtypeFreq) = colnamesPT
colnames(playtypeEff) = colnamesPT
colnames(playtypePerc) = colnamesPT

nbacolors = teamcolors %>%
  filter(league == "nba") %>%
  select(name, primary, secondary)

nbacolors[3,3] = "#FFFFFF"

abbrev = c("ATL","BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL",
           "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
nbacolors$abbrev = abbrev
nbacolors = nbacolors %>%
  select(name, abbrev, primary, secondary)

genteams = genteams %>%
  left_join(nbacolors, by = c("Team" = "abbrev"))
playtypeEff = playtypeEff %>%
  left_join(nbacolors, by = c("Team" = "abbrev"))
playtypeFreq = playtypeFreq %>%
  left_join(nbacolors, by = c("Team" = "abbrev"))
playtypePerc = playtypePerc %>%
  left_join(nbacolors, by = c("Team" = "abbrev"))
playtypes = playtypes %>%
  left_join(nbacolors, by = c("Team" = "abbrev"))

color_map = c("ATL" = "#E13A3E",
              "BOS" = "#008348",
              "BKN" = "#061922",
              "CHA" = "#008ca8",
              "CHI" = "#ce1141",
              "CLE" = "#860038",
              "DAL" = "#007dc5",
              "DEN" = "#4d90cd",
              "DET" = "#ed174c",
              "GSW" = "#fdb927",
              "HOU" = "#ce1141",
              "IND" = "#ffc633",
              "LAC" = "#ed174c",
              "LAL" = "#fdb927",
              "MEM" = "#0f586c",
              "MIA" = "#98002e",
              "MIL" = "#00471b",
              "MIN" = "#005083",
              "NOP" = "#002b5c",
              "NYK" = "#f58426",
              "OKC" = "#007dc3",
              "ORL" = "#007dc5",
              "PHI" = "#ed174c",
              "PHX" = "#e56020",
              "POR" = "#e03a3e",
              "SAC" = "#724c9f",
              "SAS" = "#bac3c9",
              "TOR" = "#ce1141",
              "UTA" = "#002b5c",
              "WAS" = "#e31837")
