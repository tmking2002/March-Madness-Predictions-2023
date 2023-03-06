library(tidyverse)

file_names <- list.files("data")

files <- lapply(paste0("data/",file_names), read.csv)
names(files) <- str_remove(file_names, ".csv")

## MENS

mens_scores <- rbind(files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(str_remove_all(names(.),"W|L")),
                    files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(str_remove_all(names(.),"W|L")))

mens_scores <- cbind(mens_scores, rbind(files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))),
                                      files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))))) %>% 
  merge(files$MTeamConferences, by = c("Season","TeamID")) %>% 
  merge(files$MTeamConferences %>% rename(opp_ConfAbbrev = ConfAbbrev), by.x = c("Season","opp_TeamID"), by.y = c("Season","TeamID"))

mens_conf_stats <- mens_scores %>% 
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
  merge(mens_conf_stats, by = c("Season","ConfAbbrev")) %>% 
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

## WOMENS

womens_scores <- rbind(files$WRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(str_remove_all(names(.),"W|L")),
                       files$WRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(str_remove_all(names(.),"W|L")))

womens_scores <- cbind(womens_scores, rbind(files$WRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))),
                                            files$WRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))))) %>% 
  merge(files$WTeamConferences, by = c("Season","TeamID")) %>% 
  merge(files$WTeamConferences %>% rename(opp_ConfAbbrev = ConfAbbrev), by.x = c("Season","opp_TeamID"), by.y = c("Season","TeamID"))

womens_conf_stats <- womens_scores %>% 
  group_by(Season, ConfAbbrev, opp_ConfAbbrev) %>% 
  summarise(ppg = mean(Score),
            opp_ppg = mean(opp_Score),
            conf_ppg_diff = ppg - opp_ppg)

womens_stats <- womens_scores %>% 
  mutate(win = Score > opp_Score,
         margin = Score - opp_Score) %>% 
  group_by(TeamID, Season, ConfAbbrev) %>% 
  summarise(win_perc = mean(win),
            avg_margin = mean(margin))

womens_tournament_conf_stats <- womens_stats %>% 
  merge(womens_conf_stats, by = c("Season","ConfAbbrev")) %>% 
  select(Season, ConfAbbrev, opp_ConfAbbrev, conf_ppg_diff) %>% 
  distinct()

womens_tourney_results <- files$WNCAATourneyDetailedResults %>% 
  select(Season, WTeamID, LTeamID, WScore, LScore) %>% 
  merge(womens_stats, by.x = c("Season","WTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(Wwin_perc = win_perc,
         Wavg_margin = avg_margin,
         WConfAbbrev = ConfAbbrev) %>% 
  merge(womens_stats, by.x = c("Season","LTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(Lwin_perc = win_perc,
         Lavg_margin = avg_margin,
         LConfAbbrev = ConfAbbrev) %>% 
  mutate(win_perc_diff = Wwin_perc - Lwin_perc,
         margin_diff = Wavg_margin - Lavg_margin) %>% 
  merge(womens_tournament_conf_stats, by.x = c("Season","WConfAbbrev","LConfAbbrev"), by.y = c("Season","ConfAbbrev","opp_ConfAbbrev"))

womens_tourney_results_longer <- rbind(womens_tourney_results %>% mutate(win = 1, score_diff = WScore - LScore),
                                     womens_tourney_results[c(1,3,2,5,4,7,6,9,8,10,11,12,13,14)] %>% mutate(win_perc_diff = -win_perc_diff,
                                                                                                    margin_diff = -margin_diff,
                                                                                                    conf_ppg_diff = -conf_ppg_diff,
                                                                                                    score_diff = LScore - WScore,
                                                                                                    win = 0))

womens_tourney_test <- womens_tourney_results_longer %>% 
  filter(Season %in% c(2017, 2018, 2019, 2021, 2022))

womens_tourney_train <- womens_tourney_results_longer %>% 
  filter(!(Season %in% c(2017, 2018, 2019, 2021, 2022)))

womens_model <- glm(score_diff ~ margin_diff + win_perc_diff + conf_ppg_diff, data = womens_tourney_train, family = "gaussian")
womens_point_sd <- sd(c(womens_tourney_train$WScore, womens_tourney_train$LScore))

womens_tourney_test$predicted_point_diff <- predict(womens_model, womens_tourney_test, type = "response")
womens_tourney_test$pred <- pnorm(womens_tourney_test$predicted_point_diff, 0, womens_point_sd)

womens_tourney_test$Resid <- womens_tourney_test$win - womens_tourney_test$pred

mean(womens_tourney_test[1:554,]$Resid)

