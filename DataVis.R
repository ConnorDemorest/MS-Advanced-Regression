# Data visualizations for the write up

library(tidyverse)
Kenpom = read_csv("NCAA2021_Kenpom.csv")
bball = read_csv("bbtraindata.csv")


bball2 <- read_csv("MNCAA2021_data/MNCAATourneyCompactResults.csv") %>% 
  filter(DayNum >= 136) %>% 
  inner_join(read_csv("MNCAA2021_data/MTeams.csv"), by = c("WTeamID" = "TeamID")) %>%
  mutate(WTeam = TeamName, .keep = "unused") %>%
  inner_join(read_csv("MNCAA2021_data/MTeams.csv"), by = c("LTeamID" = "TeamID")) %>%
  mutate(LTeam = TeamName, .keep = "unused") %>% 
  select(-c(WScore, LScore, WLoc, NumOT, FirstD1Season.x, FirstD1Season.y, LastD1Season.x, LastD1Season.y)) %>% 
  inner_join(read_csv("MNCAA2021_data/MNCAATourneySeeds.csv"), by = c("Season", "WTeamID" = "TeamID")) %>% 
  mutate(WSeed = as.numeric(substr(Seed, 2, 3)), .keep = "unused") %>%
  inner_join(read_csv("MNCAA2021_data/MNCAATourneySeeds.csv"), by = c("Season", "LTeamID" = "TeamID")) %>% 
  mutate(LSeed = as.numeric(substr(Seed, 2, 3)), Seed_diff = WSeed - LSeed, Seed = NULL)

# Kenpom rankings tend to track seeding fairly well
ggplot(data = Kenpom, aes(x = ncaa_seed, y = rank)) + 
  geom_point() + 
  geom_smooth()

# There are a few outliers of seeding compared to where they 
# are ranked according to Kenpom in 2021, but mostly the same trend holds
ggplot(data = Kenpom %>% filter(Season == 2021), aes(x = ncaa_seed, y = rank)) + 
  geom_point() + 
  geom_smooth()

# Proportion of upsets each season, upset being team at least 2 seeds worse winning
ggplot(bball2 %>% group_by(Season) %>% count(upset = Seed_diff > 1) %>% filter(upset == TRUE)) + 
  geom_bar(aes(x = Season, y = n/63), stat = "identity", fill = "steelblue") + 
  labs(y = "Proportion of upsets",
       title = "Proportion of upsets in NCAA tournament each season since 1985",
       subtitle = "Upset: A team seeded 2 seeds or worse winning (e.g. 10 seed beats 8 seed)",
       caption = "Most years between 15 and 25 percent of games are won by a lower seeded team") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Proportion of large upsets each season, large upset being team at least 5 seeds worse winning
ggplot(bball2 %>%  group_by(Season) %>% count(upset = Seed_diff > 5) %>% filter(upset == TRUE)) + 
  geom_bar(aes(x = Season, y = n/63), stat = "identity", fill = "steelblue") +
  labs(y = "Proportion of big upsets",
       title = "Proportion of big upsets each season since 1985",
       subtitle = '"Big Upset": A team seeded 6 seeds or worse winning (e.g. 8 seed beats 1 seed)',
       caption = "Most years between 5 and 15 percent of games are huge upsets") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Finding Cinderella teams that won at least 3 games when seeded 7 or lower
ggplot(bball2 %>%
         filter(WSeed >= 7, Season >= 2010) %>% 
         group_by(Season, WTeam, WSeed) %>% count() %>% filter(n >= 3)) + 
  geom_text(aes(x = Season, y = as.factor(n), label = WTeam, col = as.factor(WSeed)), 
            position = position_jitter(width=0, height=0.2), size = 5) + 
  labs(y = "Number of games won",
       color = "Team Seed",
       title = "Cinderella teams in NCAA tournament since 2010",
       subtitle = "Cinderella team: Teams that won 3+ games as a 7 seed or worse",
       caption = "Most seasons have a team that made a run in the tournament as a low seed.") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Common matchups and their chance of being an upset
bball2 %>% group_by(WSeed, LSeed) %>%
  tally() %>% 
  filter(WSeed <= 8, WSeed + LSeed == 17) %>%
  mutate(prob = n/((max(bball2$Season) - min(bball2$Season) + 1)*4)) %>%
  ggplot(aes(x = paste(WSeed, "vs", LSeed), y = prob)) +
  geom_col(fill = "steelblue")+ 
  geom_errorbar(aes(ymin = prob - sd(prob), 
                    ymax = pmin(prob + sd(prob), 1)), width = 0.2) +
  labs(x = "Matchup",
       y = "Probability higher seed wins",
       title = "Probability of higher seed winning for possible first round matchups",
       caption = "Larger disparaties in seeds is associated with higher probability of the high seed winning")  +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# 
# # Are 11 seeds that play a play in game much more likely to win their opening game?
# data2 <- bball %>% mutate(Seed.a = as.numeric(substr(SeedA, 2, 3)), Seed.b = as.numeric(substr(SeedB, 2, 3))) %>%
#   filter((Seed.a == 11 & Seed.b == 6) | (Seed.a == 6 & Seed.b == 11), WinA == 1, Season >= 2011) %>%
#   mutate(PlayIn = ifelse(substr(SeedA, 4, 4) %in% c("a", "b") | substr(SeedB, 4, 4) %in% c("a", "b"), 1, 0)) %>%
#   select(Season, A.team, B.team, PlayIn, Seed.a, Seed.b)
# df = as.tibble(with(data2, xtabs( ~ PlayIn + (Seed.a == 11))))
# ggplot(data= df) + 
#   geom_col(aes(x = PlayIn, fill = `Seed.a == 11`, y = n), position = "fill") +
#   labs(x = "Play in Team",
#        y = "",
#        fill = "11 seed won",
#        caption = "Whether a team was a play in game winner doesn't affect their chance of winning the first round game") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))




            