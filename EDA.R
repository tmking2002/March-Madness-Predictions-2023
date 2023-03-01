library(tidyverse)

file_names <- list.files("data")

files <- lapply(paste0("data/",file_names), read.csv)
names(files) <- str_remove(file_names, ".csv")

mens_scores <- rbind(files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(str_remove_all(names(.),"W|L")),
                    files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(str_remove_all(names(.),"W|L")))

mens_scores <- cbind(mens_scores, rbind(files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))),
                                      files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))))) %>% 
  merge(files$MTeamConferences, by = c("Season","TeamID")) %>% 
  merge(files$MTeamConferences %>% rename(opp_ConfAbbrev = ConfAbbrev), by.x = c("Season","opp_TeamID"), by.y = c("Season","TeamID"))

conf_stats <- mens_scores %>% 
  group_by(Season, ConfAbbrev, opp_ConfAbbrev) %>% 
  summarise(ppg = mean(Score),
            opp_ppg = mean(opp_Score),
            conf_ppg_diff = ppg - opp_ppg)

mens_ranking_data <- files$MMasseyOrdinals %>% 
  group_by(Season) %>% 
  filter(SystemName == "POM" & RankingDayNum == max(RankingDayNum)) %>% 
  ungroup() %>% 
  merge(files$MTeamConferences, by = c("Season","TeamID")) %>% 
  select(-c(SystemName, RankingDayNum))

mens_tournament_conf_stats <- mens_ranking_data %>% 
  merge(conf_stats, by = c("Season","ConfAbbrev")) %>% 
  select(Season, ConfAbbrev, opp_ConfAbbrev, conf_ppg_diff) %>% 
  distinct()

mens_tourney_results <- files$MNCAATourneyDetailedResults %>% 
  select(Season, WTeamID, LTeamID, WScore, LScore) %>% 
  merge(mens_ranking_data, by.x = c("Season","WTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(WRank = OrdinalRank,
         WConfAbbrev = ConfAbbrev) %>% 
  merge(mens_ranking_data, by.x = c("Season","LTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(LRank = OrdinalRank,
         LConfAbbrev = ConfAbbrev) %>% 
  mutate(rank_diff = WRank - LRank,
         score_diff = WScore - LScore) %>% 
  merge(mens_tournament_conf_stats, by.x = c("Season","WConfAbbrev","LConfAbbrev"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"))

mens_tourney_results_longer <- rbind(mens_tourney_results %>% mutate(win = 1),
                                     mens_tourney_results[c(1,3,2,5,4,7,6,9,8,10,11,12)] %>% mutate(rank_diff = -rank_diff,
                                                                                                    score_diff = -score_diff,
                                                                                                    conf_ppg_diff = -conf_ppg_diff,
                                                                                                    win = 0))

mens_tourney_test <- mens_tourney_results_longer %>% 
  filter(Season %in% c(2017, 2018, 2019, 2021, 2022))

mens_tourney_train <- mens_tourney_results_longer %>% 
  filter(!(Season %in% c(2017, 2018, 2019, 2021, 2022)))

rank_model <- glm(score_diff ~ rank_diff + rank_diff * conf_ppg_diff, data = mens_tourney_train, family = "gaussian")
point_sd <- sd(c(mens_tourney_train$WScore, mens_tourney_train$LScore))

mens_tourney_test$predicted_point_diff <- predict(rank_model, mens_tourney_test, type = "response")
mens_tourney_test$pred <- pnorm(mens_tourney_test$predicted_point_diff, 0, point_sd)

mens_tourney_test$Resid <- mens_tourney_test$win - mens_tourney_test$pred

mean(mens_tourney_test[1:303,]$Resid)

mens_submission <- mens_tourney_test %>% 
  mutate(ID = paste0(Season,"-",WTeamID,"-",LTeamID)) %>% 
  select(ID, pred)

