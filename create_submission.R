source("EDA.R")

teams <- files$MRegularSeasonCompactResults %>% 
  filter(Season %in% c(2017:2019,2021:2022)) %>% 
  distinct(Season, LTeamID) %>% 
  rename(TeamID = LTeamID)


matchups <- data.frame()

for(i in unique(teams$Season)){
  
  curr_matchups <- expand.grid(teams %>% filter(Season == i) %>% pull(TeamID),
                               teams %>% filter(Season == i) %>% pull(TeamID))
  
  matchups <- rbind(matchups, curr_matchups %>% mutate(season = i))
  
}

matchups <- matchups %>% 
  filter(Var1 != Var2) %>% 
  rename(Team1 = Var1,
         Team2 = Var2)

team_data <- matchups %>% 
  merge(mens_ranking_data, by.x = c("season","Team1"), by.y = c("Season","TeamID")) %>% 
  rename(Rank1 = OrdinalRank,
         Conf1 = ConfAbbrev) %>% 
  merge(mens_ranking_data, by.x = c("season","Team2"), by.y = c("Season","TeamID")) %>% 
  rename(Rank2 = OrdinalRank,
         Conf2 = ConfAbbrev) %>% 
  merge(conf_stats, by.x = c("season","Conf1","Conf2"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"), all = T) %>% 
  filter(season %in% c(2017, 2018, 2019, 2021, 2022)) %>% 
  mutate(conf_ppg_diff = ifelse(is.na(conf_ppg_diff), 0, conf_ppg_diff),
         rank_diff = Rank1 - Rank2)

test <- team_data %>% select(season, Team1, Team2)


team_data$predicted_point_diff <- predict(rank_model, team_data, type = "response")
team_data$pred <- pnorm(team_data$predicted_point_diff, 0, point_sd)
