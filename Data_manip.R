library(tidyverse)
library(forcats)
library(reshape2)

MConferenceTourneyGames <- read_csv("ncaam-march-mania-2021/MConferenceTourneyGames.csv")
MNCAATourneyCompactResults <- read_csv("ncaam-march-mania-2021/MNCAATourneyCompactResults.csv")
MNCAATourneySeeds <- read_csv("ncaam-march-mania-2021/MNCAATourneySeeds.csv")
MNCAATourneySlots <- read_csv("ncaam-march-mania-2021/MNCAATourneySlots.csv")
MRegularSeasonCompactResults <- read_csv("ncaam-march-mania-2021/MRegularSeasonCompactResults.csv")
MRegularSeasonDetailedResults <- read_csv("ncaam-march-mania-2021/MRegularSeasonDetailedResults.csv")
MTeams <- read_csv("ncaam-march-mania-2021/MTeams.csv")
NCAA2021_Kenpom <- read_csv("NCAA2021_Kenpom.csv")

# Filter only since 2005 data
# Double the data so there are both 1's and 0's in the data
data = MNCAATourneyCompactResults %>% 
  filter(Season >= 2005 & !(DayNum %in% c(134,135))) %>% 
  select(Season, WTeamID, LTeamID, DayNum, WScore, LScore) %>% 
  left_join(MNCAATourneySeeds, by = c("Season", "WTeamID" = "TeamID")) %>% 
  left_join(MNCAATourneySeeds, by = c("Season", "LTeamID" = "TeamID")) %>% 
  mutate(TeamIdA = WTeamID, TeamIdB = LTeamID,
         SeedA = Seed.x, SeedB = Seed.y,
         ScoreA = WScore, ScoreB = LScore,
         Round = fct_collapse(as.factor(DayNum), 
                              "1" = c("136", "137"),
                              "2" = c("138", "139"), 
                              "3" = c("143", "144"),
                              "4" = c("145", "146"),
                              "5" = "152",
                              "6" = "154"),
         .keep = "unused")
data2 = MNCAATourneyCompactResults %>% 
  filter(Season >= 2005 & !(DayNum %in% c(134,135))) %>% 
  select(Season, WTeamID, LTeamID, DayNum, WScore, LScore) %>% 
  left_join(MNCAATourneySeeds, by = c("Season", "WTeamID" = "TeamID")) %>% 
  left_join(MNCAATourneySeeds, by = c("Season", "LTeamID" = "TeamID")) %>% 
  mutate(TeamIdA = LTeamID, TeamIdB = WTeamID,
         SeedA = Seed.y, SeedB = Seed.x,
         ScoreA = LScore, ScoreB = WScore,
         Round = fct_collapse(as.factor(DayNum), 
                              "1" = c("136", "137"),
                              "2" = c("138", "139"), 
                              "3" = c("143", "144"),
                              "4" = c("145", "146"),
                              "5" = "152",
                              "6" = "154"),
         .keep = "unused") %>% 
  mutate(Round = fct_collapse(as.factor(DayNum), 
                              "1" = c("136", "137"),
                              "2" = c("138", "139"), 
                              "3" = c("143", "144"),
                              "4" = c("145", "146"),
                              "5" = "152",
                              "6" = "154"))

bball_data = rbind(data, data2) %>% 
  mutate(Seed_diff = as.numeric(substring(SeedA,2,3)) - as.numeric(substring(SeedB,2,3)),
         # now we have two outcomes, Score diff and WinA
         Score_diff = ScoreA - ScoreB, 
         WinA = as.factor(ifelse(Score_diff > 0, 1, 0)),
  ) %>% 
# Get Kenpom data for winning teams
# Kenpom gives data for offense, defense, overall, adjusted to 100 possessions
# sos is strength of schedule, so adjusting by the teams that they played
# nc is Non-conference, so the nc_sos is adjusted by the nc schedule for each team
# em is efficiency margin, the difference between offense eff and defense eff scaled to 100 possessions,
# Positive em is better offense than defense. 
  left_join(NCAA2021_Kenpom, by = c("Season", "TeamIdA" = "TeamID")) %>% 
  select(-c(TeamName, FirstD1Season, LastD1Season, Seed, ncaa_seed)) %>% 
  mutate(A.rank = rank, A.team = team, A.conference = conference,  
         A.record = record, 
         A.Wins = colsplit(A.record, "-", names = c("WinsA", " LossesA"))[,1],
         A.Losses = colsplit(A.record, "-", names = c("WinsA", " LossesA"))[,2],
         A.WinPerc = A.Wins/(A.Wins + A.Losses),
         A.adj_em = adj_em, A.adj_o = adj_o, A.adj_o_rank = adj_o_rank, A.adj_d = adj_d, A.adj_d_rank = adj_d_rank, 
         A.adj_tempo = adj_tempo, A.adj_tempo_rank = adj_tempo_rank, A.luck = luck, A.luck_rank = luck_rank, 
         A.sos_adj_em = sos_adj_em,
         A.sos_adj_em_rank = sos_adj_em_rank, A.sos_adj_o = sos_adj_o, A.sos_adj_o_rank = sos_adj_o_rank,
         A.sos_adj_d = sos_adj_d, A.sos_adj_d_rank = sos_adj_d_rank, A.nc_sos_adj_em = nc_sos_adj_em,
         A.nc_sos_adj_em_rank = nc_sos_adj_em_rank,
         .keep = "unused") %>% 
  # get Kenpom data for losing teams 
  left_join(NCAA2021_Kenpom, by = c("Season", "TeamIdB" = "TeamID")) %>% 
  select(-c(TeamName, FirstD1Season, LastD1Season, Seed, ncaa_seed)) %>% 
  mutate(B.rank = rank, B.team = team, B.conference = conference, B.record = record,
         B.Wins = colsplit(B.record, "-", names = c("WinsB", " LossesB"))[,1],
         B.Losses = colsplit(B.record, "-", names = c("WinsB", " LossesB"))[,2],
         B.WinPerc = B.Wins/(B.Wins + B.Losses),
         B.adj_em = adj_em, B.adj_o = adj_o, B.adj_o_rank = adj_o_rank, B.adj_d = adj_d, B.adj_d_rank = adj_d_rank, 
         B.adj_tempo = adj_tempo, B.adj_tempo_rank = adj_tempo_rank, B.luck = luck, B.luck_rank = luck_rank, 
         B.sos_adj_em = sos_adj_em,
         B.sos_adj_em_rank = sos_adj_em_rank, B.sos_adj_o = sos_adj_o, B.sos_adj_o_rank = sos_adj_o_rank,
         B.sos_adj_d = sos_adj_d, B.sos_adj_d_rank = sos_adj_d_rank, B.nc_sos_adj_em = nc_sos_adj_em,
         B.nc_sos_adj_em_rank = nc_sos_adj_em_rank,
         .keep = "unused") %>% 
  # Differences between winning team and losing team
  # Rank: the overall ordinal Kenpom rankings for each team for each stat. Negative would be winning team is better
  # Diff: the actual values for each stat. Negative would be the winning team is worse
  mutate(Rank_diff = A.rank - B.rank,
         W_diff = A.Wins - B.Wins,
         L_diff = A.Losses - B.Losses,
         W_perc_diff = A.WinPerc - B.WinPerc,
         adj_em_diff = A.adj_em - B.adj_em, adj_o_diff = A.adj_o - B.adj_o, 
         adj_o_rank_diff = A.adj_o_rank - B.adj_o_rank, adj_d_diff = A.adj_d - B.adj_d, 
         adj_d_rank_diff = A.adj_d_rank - B.adj_d_rank, adj_tempo_diff = A.adj_tempo - B.adj_tempo, 
         adj_tempo_rank_diff = A.adj_tempo_rank - B.adj_tempo_rank, luck_diff = A.luck - B.luck, 
         luck_rank_diff = A.luck_rank - B.luck_rank, sos_adj_em_diff = A.sos_adj_em - B.sos_adj_em,
         sos_adj_em_rank_diff = A.sos_adj_em_rank - B.sos_adj_em_rank, sos_adj_o_diff = A.sos_adj_o - B.sos_adj_o,
         sos_adj_o_rank_diff = A.sos_adj_o_rank - B.sos_adj_o_rank, sos_adj_d_diff = A.sos_adj_d - B.sos_adj_d,
         sos_adj_d_rank_diff = A.sos_adj_d_rank - B.sos_adj_d_rank, 
         nc_sos_adj_em_diff = A.nc_sos_adj_em - B.nc_sos_adj_em,
         nc_sos_adj_em_rank_diff = A.nc_sos_adj_em_rank - B.nc_sos_adj_em_rank,
         .keep = "unused")


bbtraindata = write_csv(bball_data, "bbtraindata.csv")

# TODO: -code an outcome (result)-, -add predictors- (added KenPom data), 
# fit categorical regression on data, determine which model is best, 
# make 2021 predictions, submit by Friday morning at 10:15 MST

########################
devtools::install_github("dhutexas/collegehoops", dep = TRUE)
library(collegehoops)

# Data frame to predict when we choose and train a final model, matchups for 2021
# Right now the games are double counted (like, team 1 will play team 2, and team 2 will play team 1)
# the play in games are included as well

pred_data = read_csv("pred_data.csv") %>%
  mutate(season = as.numeric(substring(ID, 1, 4)),
         teamid_1 = as.numeric(substring(ID, 6, 9)),
         teamid_2 = as.numeric(substring(ID, 11, 14)), 
         ID = NULL, Pred = NULL) %>%
  left_join(NCAA2021_Kenpom, by = c("season" = "Season", "teamid_1" = "TeamID")) %>% 
  select(-c(TeamName, FirstD1Season, LastD1Season, ncaa_seed)) %>%
  mutate(A.rank = rank, A.team = team, A.conference = conference, A.Pos = Seed, 
         A.seed = as.numeric(substring(Seed, 2, 3)),  
         A.record = record, 
         A.Wins = colsplit(A.record, "-", names = c("WinsA", " LossesA"))[,1],
         A.Losses = colsplit(A.record, "-", names = c("WinsA", " LossesA"))[,2],
         A.WinPerc = A.Wins/(A.Wins + A.Losses),
         A.adj_em = adj_em, A.adj_o = adj_o, A.adj_o_rank = adj_o_rank, A.adj_d = adj_d, A.adj_d_rank = adj_d_rank, 
         A.adj_tempo = adj_tempo, A.adj_tempo_rank = adj_tempo_rank, A.luck = luck, A.luck_rank = luck_rank, 
         A.sos_adj_em = sos_adj_em,
         A.sos_adj_em_rank = sos_adj_em_rank, A.sos_adj_o = sos_adj_o, A.sos_adj_o_rank = sos_adj_o_rank,
         A.sos_adj_d = sos_adj_d, A.sos_adj_d_rank = sos_adj_d_rank, A.nc_sos_adj_em = nc_sos_adj_em,
         A.nc_sos_adj_em_rank = nc_sos_adj_em_rank,
         .keep = "unused") %>% 
  # get Kenpom data for losing teams 
  left_join(NCAA2021_Kenpom, by = c("season" = "Season", "teamid_2" = "TeamID")) %>% 
  select(-c(TeamName, FirstD1Season, LastD1Season, ncaa_seed)) %>% 
  mutate(B.rank = rank, B.team = team, B.conference = conference, B.Pos = Seed, 
         B.seed = as.numeric(substring(Seed, 2, 3)),
         B.record = record,
         B.Wins = colsplit(B.record, "-", names = c("WinsB", " LossesB"))[,1],
         B.Losses = colsplit(B.record, "-", names = c("WinsB", " LossesB"))[,2],
         B.WinPerc = B.Wins/(B.Wins + B.Losses),
         B.adj_em = adj_em, B.adj_o = adj_o, B.adj_o_rank = adj_o_rank, B.adj_d = adj_d, B.adj_d_rank = adj_d_rank, 
         B.adj_tempo = adj_tempo, B.adj_tempo_rank = adj_tempo_rank, B.luck = luck, B.luck_rank = luck_rank, 
         B.sos_adj_em = sos_adj_em,
         B.sos_adj_em_rank = sos_adj_em_rank, B.sos_adj_o = sos_adj_o, B.sos_adj_o_rank = sos_adj_o_rank,
         B.sos_adj_d = sos_adj_d, B.sos_adj_d_rank = sos_adj_d_rank, B.nc_sos_adj_em = nc_sos_adj_em,
         B.nc_sos_adj_em_rank = nc_sos_adj_em_rank,
         .keep = "unused") %>% 
  # Differences between winning team and losing team
  # Rank: the overall ordinal Kenpom rankings for each team for each stat. Negative would be winning team is better
  # Diff: the actual values for each stat. Negative would be the winning team is worse
  mutate(Seed_diff = A.seed - B.seed,
         Rank_diff = A.rank - B.rank,
         W_diff = A.Wins - B.Wins,
         L_diff = A.Losses - B.Losses,
         W_perc_diff = A.WinPerc - B.WinPerc,
         adj_em_diff = A.adj_em - B.adj_em, adj_o_diff = A.adj_o - B.adj_o, 
         adj_o_rank_diff = A.adj_o_rank - B.adj_o_rank, adj_d_diff = A.adj_d - B.adj_d, 
         adj_d_rank_diff = A.adj_d_rank - B.adj_d_rank, adj_tempo_diff = A.adj_tempo - B.adj_tempo, 
         adj_tempo_rank_diff = A.adj_tempo_rank - B.adj_tempo_rank, luck_diff = A.luck - B.luck, 
         luck_rank_diff = A.luck_rank - B.luck_rank, sos_adj_em_diff = A.sos_adj_em - B.sos_adj_em,
         sos_adj_em_rank_diff = A.sos_adj_em_rank - B.sos_adj_em_rank, sos_adj_o_diff = A.sos_adj_o - B.sos_adj_o,
         sos_adj_o_rank_diff = A.sos_adj_o_rank - B.sos_adj_o_rank, sos_adj_d_diff = A.sos_adj_d - B.sos_adj_d,
         sos_adj_d_rank_diff = A.sos_adj_d_rank - B.sos_adj_d_rank, 
         nc_sos_adj_em_diff = A.nc_sos_adj_em - B.nc_sos_adj_em,
         nc_sos_adj_em_rank_diff = A.nc_sos_adj_em_rank - B.nc_sos_adj_em_rank,
         .keep = "unused")

pred_data = write_csv(pred_data, "pred_data.csv")
