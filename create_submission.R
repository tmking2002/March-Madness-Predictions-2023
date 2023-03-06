source("EDA.R")

## MENS

mens_teams <- data.frame()

for(i in c(2017:2019,2021:2022)){
  
  teams <- files$MTeams %>% 
    filter(i >= FirstD1Season & i <= LastD1Season) %>% 
    pull(TeamID)
  
  mens_teams <- rbind(mens_teams, data.frame(Season = i,
                                             TeamID = teams))
}


mens_matchups <- data.frame()

for(i in unique(mens_teams$Season)){
  
  curr_matchups <- expand.grid(mens_teams %>% filter(Season == i) %>% pull(TeamID),
                               mens_teams %>% filter(Season == i) %>% pull(TeamID))
  
  mens_matchups <- rbind(mens_matchups, curr_matchups %>% mutate(season = i))
  
}

mens_matchups <- mens_matchups %>% 
  filter(Var1 < Var2) %>% 
  rename(Team1 = Var1,
         Team2 = Var2)

mens_team_data <- mens_matchups %>% 
  merge(mens_ranking_data, by.x = c("season","Team1"), by.y = c("Season","TeamID")) %>% 
  rename(Rank1 = OrdinalRank,
         Conf1 = ConfAbbrev) %>% 
  merge(mens_ranking_data, by.x = c("season","Team2"), by.y = c("Season","TeamID")) %>% 
  rename(Rank2 = OrdinalRank,
         Conf2 = ConfAbbrev) %>% 
  merge(mens_conf_stats, by.x = c("season","Conf1","Conf2"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"), all = T) %>% 
  filter(season %in% c(2017, 2018, 2019, 2021, 2022)) %>% 
  mutate(conf_ppg_diff = ifelse(is.na(conf_ppg_diff), 0, conf_ppg_diff),
         rank_diff = Rank1 - Rank2)

mens_team_data$predicted_point_diff <- predict(rank_model, mens_team_data, type = "response")
mens_team_data$pred <- pnorm(mens_team_data$predicted_point_diff, 0, point_sd)

mens_submission <- mens_team_data %>% 
  mutate(ID = paste0(season,"_",Team1,"_",Team2)) %>% 
  select(ID, pred)

## WOMENS

womens_teams <- data.frame()

for(i in c(2017:2019,2021,2022)){
  
  teams <- files$WTeams %>% 
    pull(TeamID)
  
  womens_teams <- rbind(womens_teams, data.frame(Season = i,
                                             TeamID = teams))
}

womens_matchups <- data.frame()

for(i in unique(womens_teams$Season)){
  
  curr_matchups <- expand.grid(womens_teams %>% filter(Season == i) %>% pull(TeamID),
                               womens_teams %>% filter(Season == i) %>% pull(TeamID))
  
  womens_matchups <- rbind(womens_matchups, curr_matchups %>% mutate(season = i))
  
}

womens_matchups <- womens_matchups %>% 
  filter(Var1 < Var2) %>% 
  rename(Team1 = Var1,
         Team2 = Var2)

womens_team_data <- womens_matchups %>% 
  merge(womens_stats, by.x = c("season","Team1"), by.y = c("Season","TeamID")) %>% 
  rename(win_perc_1 = win_perc,
         margin_1 = avg_margin,
         Conf1 = ConfAbbrev) %>% 
  merge(womens_stats, by.x = c("season","Team2"), by.y = c("Season","TeamID")) %>% 
  rename(win_perc_2 = win_perc,
         margin_2 = avg_margin,
         Conf2 = ConfAbbrev) %>% 
  merge(womens_conf_stats, by.x = c("season","Conf1","Conf2"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"), all = T) %>% 
  filter(season %in% c(2017, 2018, 2019, 2021, 2022)) %>% 
  mutate(conf_ppg_diff = ifelse(is.na(conf_ppg_diff), 0, conf_ppg_diff),
         win_perc_diff = win_perc_1 - win_perc_2,
         margin_diff = margin_1 - margin_2)

womens_team_data$predicted_point_diff <- predict(womens_model, womens_team_data, type = "response")
womens_team_data$pred <- pnorm(womens_team_data$predicted_point_diff, 0, womens_point_sd)

womens_submission <- womens_team_data %>% 
  mutate(ID = paste0(season,"_",Team1,"_",Team2)) %>% 
  select(ID, pred)

submission <- rbind(womens_submission, mens_submission) %>% 
  arrange(ID)

