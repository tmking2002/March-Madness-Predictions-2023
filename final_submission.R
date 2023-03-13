source("EDA.R")

## MENS


mens_teams <- files$MTeams %>% 
  filter(2023 >= FirstD1Season & 2023 <= LastD1Season) %>% 
  pull(TeamID)

mens_matchups <- expand.grid(mens_teams, mens_teams)


mens_matchups <- mens_matchups %>% 
  filter(Var1 < Var2) %>% 
  rename(Team1 = Var1,
         Team2 = Var2) %>% 
  mutate(season = 2023)

mens_team_data <- mens_matchups %>% 
  merge(mens_ranking_data, by.x = c("season","Team1"), by.y = c("Season","TeamID")) %>% 
  rename(Rank1 = OrdinalRank,
         Conf1 = ConfAbbrev) %>% 
  merge(mens_ranking_data, by.x = c("season","Team2"), by.y = c("Season","TeamID")) %>% 
  rename(Rank2 = OrdinalRank,
         Conf2 = ConfAbbrev) %>% 
  merge(mens_conf_stats, by.x = c("season","Conf1","Conf2"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"), all = T) %>% 
  mutate(conf_ppg_diff = ifelse(is.na(conf_ppg_diff), 0, conf_ppg_diff),
         rank_diff = Rank1 - Rank2) %>% 
  filter(season == 2023)

mens_team_data$predicted_point_diff <- predict(rank_model, mens_team_data, type = "response")
mens_team_data$pred <- pnorm(mens_team_data$predicted_point_diff, 0, point_sd)

mens_submission <- mens_team_data %>% 
  mutate(ID = paste0(season,"_",Team1,"_",Team2)) %>% 
  select(ID, pred)

## WOMENS

womens_teams <- files$WTeams %>% 
  pull(TeamID)

womens_matchups <- expand.grid(womens_teams, womens_teams)


womens_matchups <- womens_matchups %>% 
  filter(Var1 < Var2) %>% 
  rename(Team1 = Var1,
         Team2 = Var2) %>% 
  mutate(season = 2023)

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
  mutate(conf_ppg_diff = ifelse(is.na(conf_ppg_diff), 0, conf_ppg_diff),
         win_perc_diff = win_perc_1 - win_perc_2,
         margin_diff = margin_1 - margin_2) %>% 
  filter(season == 2023)

womens_team_data$predicted_point_diff <- predict(womens_model, womens_team_data, type = "response")
womens_team_data$pred <- pnorm(womens_team_data$predicted_point_diff, 0, womens_point_sd)

womens_submission <- womens_team_data %>% 
  mutate(ID = paste0(season,"_",Team1,"_",Team2)) %>% 
  select(ID, pred)

submission <- rbind(womens_submission, mens_submission) %>% 
  arrange(ID) %>% 
  drop_na()

write_csv(submission, "final_submission_2023.csv")
