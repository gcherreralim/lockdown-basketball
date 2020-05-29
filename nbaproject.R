library(tidyverse)
options(scipen=999)


files = list.files(path = "/Users/GabbyHL/Desktop/NBA-lineupscraping-trial/team-scrape/",
                   pattern = "*.csv")
teams = lapply(paste0("/Users/GabbyHL/Desktop/NBA-lineupscraping-trial/team-scrape/",files), read_csv) %>% bind_rows()

teams[teams$team == "MilwaukeeBucks",1] = "MIL"
teams[teams$team == "LosAngelesLakers",1] = "LAL"
teams[teams$team == "TorontoRaptors",1] = "TOR"
teams[teams$team == "LAClippers",1] = "LAC"
teams[teams$team == "BostonCeltics",1] = "BOS"
teams[teams$team == "DenverNuggets",1] = "DEN"
teams[teams$team == "MiamiHeat",1] = "MIA"
teams[teams$team == "UtahJazz",1] = "UTA"
teams[teams$team == "DallasMavericks",1] = "DAL"
teams[teams$team == "HoustonRockets",1] = "HOU"
teams[teams$team == "OklahomaCityThunder",1] = "OKC"
teams[teams$team == "IndianaPacers",1] = "IND"
teams[teams$team == "PhiladelphiaSixers",1] = "PHI"
teams[teams$team == "MemphisGrizzlies",1] = "MEM"
teams[teams$team == "BrooklynNets",1] = "BKN"
teams[teams$team == "OrlandoMagic",1] = "ORL"
teams[teams$team == "PortlandTrailBlazers",1] = "POR"
teams[teams$team == "NewOrleansPelicans",1] = "NOP"
teams[teams$team == "SacramentoKings",1] = "SAC"
teams[teams$team == "SanAntonioSpurs",1] = "SAS"
teams[teams$team == "PhoenixSuns",1] = "PHX"
teams[teams$team == "WashingtonWizards",1] = "WAS"
teams[teams$team == "CharlotteHornets",1] = "CHA"
teams[teams$team == "ChicagoBulls",1] = "CHI"
teams[teams$team == "NewYorkKnicks",1] = "NYK"
teams[teams$team == "AtlantaHawks",1] = "ATL"
teams[teams$team == "DetroitPistons",1] = "DET"
teams[teams$team == "ClevelandCavaliers",1] = "CLE"
teams[teams$team == "MinnesotaTimberwolves",1] = "MIN"
teams[teams$team == "GoldenStateWarriors",1] = "GSW"

teams = teams %>%
  mutate(season = season - 1)

write_csv(teams, path = "/Users/GabbyHL/Desktop/NBA-lineupscraping-trial/NBA Project/playtypes.csv", na = "NA", col_names = TRUE)

stuffer = read_csv("/Users/GabbyHL/Desktop/NBA-lineupscraping-trial/NBA Project/NBAData20152016to20192020.csv")
stuffer[stuffer$Team == "GS",1] = "GSW"
teams = teams %>%
  mutate(teamcode = paste0(team,season))

stuffer = stuffer %>%
  mutate(teamcode = paste0(Team,Season))

fullteams = teams %>%
  inner_join(stuffer, by = "teamcode")

write_csv(fullteams, path = "/Users/GabbyHL/Desktop/NBA-lineupscraping-trial/NBA Project/fullteams.csv", na = "NA", col_names = TRUE)

hello = "hello"