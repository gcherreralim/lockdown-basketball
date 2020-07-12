### USED PACKAGES
pack = c("shiny", "ggplot2", "magrittr", "reactable","tidyverse", "httr", "stringr", "lubridate", "plotly",
         "shinydashboard", "caret", "formattable", "data.table", "highcharter",
         "htmltools", "shinyjs", "leaflet", "reshape2", "class", 
         "FNN", "teamcolors", "ggrepel", "extrafont", "showtext","scales")

options(scipen = 999)
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
fullteams = fullteams %>%
  mutate(SeasonRange = as.factor(SeasonRange))

# Final Large Datasets
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

# Color Scheme
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

# For Season/Window Amount Diff
genteamsWA_A = genteams %>%
  select(TeamCode, Team, Season, SeasonRange, Pace, PPG, OppPPG, oEFF, dEFF, EFFDiff, OffRtg, DefRtg, NetRtg)

genteams_g1ave1 = genteamsWA_A %>%
  group_by(Season) %>%
  dplyr::summarize(PaceAve1 = mean(Pace),
                   PPGAve1 = mean(PPG),
                   OppPPGAve1 = mean(OppPPG),
                   oEFFAve1 = mean(oEFF),
                   dEFFAve1 = mean(dEFF),
                   EFFDiffAve1 = mean(EFFDiff),
                   OffRtgAve1 = mean(OffRtg),
                   DefRtgAve1 = mean(DefRtg),
                   NetRtgAve1 = mean(NetRtg)
  )

genteams_g1ave5 = genteamsWA_A %>%
  dplyr::mutate(PaceAve5 = mean(Pace),
                PPGAve5 = mean(PPG),
                OppPPGAve5 = mean(OppPPG),
                oEFFAve5 = mean(oEFF),
                dEFFAve5 = mean(dEFF),
                EFFDiffAve5 = mean(EFFDiff),
                OffRtgAve5 = mean(OffRtg),
                DefRtgAve5 = mean(DefRtg),
                NetRtgAve5 = mean(NetRtg)
  ) %>%
  select(Season, PaceAve5, PPGAve5, OppPPGAve5, oEFFAve5, dEFFAve5, EFFDiffAve5, OffRtgAve5, DefRtgAve5, NetRtgAve5)

genteamsWA_A = genteamsWA_A %>%
  left_join(genteams_g1ave1, by = "Season") %>%
  left_join(genteams_g1ave5, by = "Season") %>%
  distinct() %>%
  mutate(PaceDiff1 = ifelse(Pace > PaceAve1, Pace-PaceAve1, -(PaceAve1-Pace)),
         PaceDiff5 = ifelse(Pace > PaceAve5, Pace-PaceAve5, -(PaceAve5-Pace)),
         PPGDiff1 = ifelse(PPG > PPGAve1, PPG-PPGAve1, -(PPGAve1-PPG)),
         PPGDiff5 = ifelse(PPG > PPGAve5, PPG-PPGAve5, -(PPGAve5-PPG)),
         OppPPGDiff1 = ifelse(OppPPG > OppPPGAve1, -(OppPPG-OppPPGAve1), OppPPGAve1-OppPPG),
         OppPPGDiff5 = ifelse(OppPPG > OppPPGAve5, -(OppPPG-OppPPGAve5), OppPPGAve5-OppPPG),
         oEFFDiff1 = ifelse(oEFF > oEFFAve1, oEFF-oEFFAve1, -(oEFFAve1-oEFF)),
         oEFFDiff5 = ifelse(oEFF > oEFFAve5, oEFF-oEFFAve5, -(oEFFAve5-oEFF)),
         dEFFDiff1 = ifelse(dEFF > dEFFAve1, -(dEFF-dEFFAve1), dEFFAve1-dEFF),
         dEFFDiff5 = ifelse(dEFF > dEFFAve5, -(dEFF-dEFFAve5), dEFFAve5-dEFF),
         EFFDiffDiff1 = ifelse(EFFDiff > EFFDiffAve1, EFFDiff-EFFDiffAve1, -(EFFDiffAve1-EFFDiff)),
         EFFDiffDiff5 = ifelse(EFFDiff > EFFDiffAve5, EFFDiff-EFFDiffAve5, -(EFFDiffAve5-EFFDiff)),
         OffRtgDiff1 = ifelse(OffRtg > OffRtgAve1, OffRtg-OffRtgAve1, -(OffRtgAve1-OffRtg)),
         OffRtgDiff5 = ifelse(OffRtg > OffRtgAve5, OffRtg-OffRtgAve5, -(OffRtgAve5-OffRtg)),
         DefRtgDiff1 = ifelse(DefRtg > DefRtgAve1, -(DefRtg-DefRtgAve1), DefRtgAve1-DefRtg),
         DefRtgDiff5 = ifelse(DefRtg > DefRtgAve5, -(DefRtg-DefRtgAve5), DefRtgAve5-DefRtg),
         NetRtgDiff1 = ifelse(NetRtg > NetRtgAve1, NetRtg-NetRtgAve1, -(NetRtgAve1-NetRtg)),
         NetRtgDiff5 = ifelse(NetRtg > NetRtgAve5, NetRtg-NetRtgAve5, -(NetRtgAve5-NetRtg))
  ) %>%
  select(c(1:4),tail(names(.), 18))


# For Season/Window Percent Diff
genteamsWA_P = genteams %>%
  select(TeamCode, Team, Season, SeasonRange, WinPerc, EstWinPerc, eFGPercSeason, TSPerc, ASTPerc, TOVPerc, ASTtoTOV, ORebPerc, RebPerc)

genteams_g2ave1 = genteamsWA_P %>%
  group_by(Season) %>%
  dplyr::summarize(WPAve1 = mean(WinPerc),
                   EWPAve1 = mean(EstWinPerc),
                   EFGAve1 = mean(eFGPercSeason),
                   TSAve1 = mean(TSPerc),
                   ASTAve1 = mean(ASTPerc),
                   TOAve1 = mean(TOVPerc),
                   ATAve1 = mean(ASTtoTOV),
                   OREBAve1 = mean(ORebPerc),
                   REBAve1 = mean(RebPerc)
  )

genteams_g2ave5 = genteamsWA_P %>%
  dplyr::mutate(WPAve5 = mean(WinPerc),
                EWPAve5 = mean(EstWinPerc),
                EFGAve5 = mean(eFGPercSeason),
                TSAve5 = mean(TSPerc),
                ASTAve5 = mean(ASTPerc),
                TOAve5 = mean(TOVPerc),
                ATAve5 = mean(ASTtoTOV),
                OREBAve5 = mean(ORebPerc),
                REBAve5 = mean(RebPerc)
  ) %>%
  select(Season, WPAve5, EWPAve5, EFGAve5, TSAve5, ASTAve5, TOAve5, ATAve5, OREBAve5, REBAve5) %>%
  distinct()

genteamsWA_P = genteamsWA_P %>%
  left_join(genteams_g2ave1, by = "Season") %>%
  left_join(genteams_g2ave5, by = "Season") %>%
  distinct() %>%
  mutate(WPDiff1 = ifelse(WinPerc > WPAve1, (WinPerc-WPAve1)/WPAve1, -(WPAve1-WinPerc)/WPAve1),
         WPDiff5 = ifelse(WinPerc > WPAve5, (WinPerc-WPAve5)/WPAve5, -(WPAve5-WinPerc)/WPAve5),
         EWPDiff1 = ifelse(EstWinPerc > EWPAve1, (EstWinPerc-EWPAve1)/EWPAve1, -(EWPAve1-EstWinPerc)/EWPAve1),
         EWPDiff5 = ifelse(EstWinPerc > EWPAve5, (EstWinPerc-EWPAve5)/EWPAve5, -(EWPAve5-EstWinPerc)/EWPAve5),
         EFGDiff1 = ifelse(eFGPercSeason > EFGAve1, (eFGPercSeason-EFGAve1)/EFGAve1, -(EFGAve1-eFGPercSeason)/EFGAve1),
         EFGDiff5 = ifelse(eFGPercSeason > EFGAve5, (eFGPercSeason-EFGAve5)/EFGAve5, -(EFGAve5-eFGPercSeason)/EFGAve5),
         TSDiff1 = ifelse(TSPerc > TSAve1, (TSPerc-TSAve1)/TSAve1, -(TSAve1-TSPerc)/TSAve1),
         TSDiff5 = ifelse(TSPerc > TSAve5, (TSPerc-TSAve5)/TSAve5, -(TSAve5-TSPerc)/TSAve5),
         ASTDiff1 = ifelse(ASTPerc > ASTAve1, (ASTPerc-ASTAve1)/ASTAve1, -(ASTAve1-ASTPerc)/ASTAve1),
         ASTDiff5 = ifelse(ASTPerc > ASTAve5, (ASTPerc-ASTAve5)/ASTAve5, -(ASTAve5-ASTPerc)/ASTAve5),
         TODiff1 = ifelse(TOVPerc > TOAve1, -(TOVPerc-TOAve1)/TOAve1, (TOAve1-TOVPerc)/TOAve1),
         TODiff5 = ifelse(TOVPerc > TOAve5, -(TOVPerc-TOAve5)/TOAve5, (TOAve5-TOVPerc)/TOAve5),
         ATDiff1 = ifelse(ASTtoTOV > ATAve1, (ASTtoTOV-ATAve1)/ATAve1, -(ATAve1-ASTtoTOV)/ATAve1),
         ATDiff5 = ifelse(ASTtoTOV > ATAve5, (ASTtoTOV-ATAve5)/ATAve5, -(ATAve5-ASTtoTOV)/ATAve5),
         OREBDiff1 = ifelse(ORebPerc > OREBAve1, (ORebPerc-OREBAve1)/OREBAve1, -(OREBAve1-ORebPerc)/OREBAve1),
         OREBDiff5 = ifelse(ORebPerc > OREBAve5, (ORebPerc-OREBAve5)/OREBAve5, -(OREBAve5-ORebPerc)/OREBAve5),
         REBDiff1 = ifelse(RebPerc > REBAve1, (RebPerc-REBAve1)/REBAve1, -(REBAve1-RebPerc)/REBAve1),
         REBDiff5 = ifelse(RebPerc > REBAve5, (RebPerc-REBAve5)/REBAve5, -(REBAve5-RebPerc)/REBAve5)
  ) %>%
  select(c(1:4),tail(names(.), 18))
