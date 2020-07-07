### USED PACKAGES
pack = c("shiny", "shinythemes", "ggplot2", "magrittr", "DT", "reactable",
         "shinyWidgets", "tidyverse", "httr", "stringr", "lubridate", "plotly",
         "shinydashboard", "caret", "formattable", "data.table", "highcharter",
         "RColorBrewer", "htmltools", "shinyjs", "leaflet", "reshape2", "class", 
         "FNN", "teamcolors", "ggrepel")

# VERIFY PACKAGES
package.check <- lapply(pack, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


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
