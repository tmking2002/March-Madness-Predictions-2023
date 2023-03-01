library(tidyverse)

file_names <- list.files("data")

files <- lapply(paste0("data/",file_names), read.csv)
names(files) <- str_remove(file_names, ".csv")

mens_stats <- rbind(files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(str_remove_all(names(.),"W|L")),
                    files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(str_remove_all(names(.),"W|L")))

mens_stats <- cbind(mens_stats, rbind(files$MRegularSeasonDetailedResults[c(1,2,5:6,8,22:34)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L"))),
                                      files$MRegularSeasonDetailedResults[c(1:4,8:21)] %>% `names<-`(paste0("opp_",str_remove_all(names(.),"W|L")))))



mens_seed_data <- merge(files$MTeams, files$MNCAATourneySeeds, by = "TeamID") %>% 
  mutate(Seed = as.numeric(str_remove_all(Seed, "\\D"))) %>% 
  select(TeamID, TeamName, Season, Seed)


mens_tourney_results <- files$MNCAATourneyDetailedResults %>% 
  select(Season, WTeamID, LTeamID) %>% 
  merge(mens_seed_data, by.x = c("Season","WTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(WSeed = Seed,
         WTeamName = TeamName) %>% 
  merge(mens_seed_data, by.x = c("Season","LTeamID"), by.y = c("Season","TeamID")) %>% 
  rename(LSeed = Seed,
         LTeamName = TeamName) %>% 
  mutate(win = ifelse(WSeed == LSeed, WTeamID > LTeamID, WSeed < LSeed),
         seed_diff = abs(LSeed - WSeed))

mens_tourney_test <- mens_tourney_results %>% 
  filter(Season %in% c(2017, 2018, 2019, 2021, 2022))

mens_tourney_train <- mens_tourney_results %>% 
  filter(!(Season %in% c(2017, 2018, 2019, 2021, 2022)))

seed_model <- glm(win ~ seed_diff, data = mens_tourney_train, family = "gaussian")

mens_tourney_test$Pred <- predict(seed_model, mens_tourney_test, type = "response")
mens_tourney_test$Resid <- 1 - mens_tourney_test$Pred

mean(mens_tourney_test$Resid)

mens_submission <- mens_tourney_test %>% 
  mutate(ID = paste0(Season,"-",WTeamID,"-",LTeamID)) %>% 
  select(ID, Pred)

