library(tidyverse)

pred <- read_csv("final_submission_2023.csv")
teams <- read_csv("data/WTeams.csv")

predict <- function(team_name_1, team_name_2){
  
  team_id_1 <- teams %>% 
    filter(TeamName == team_name_1) %>% 
    pull(TeamID)
  
  team_id_2 <- teams %>% 
    filter(TeamName == team_name_2) %>% 
    pull(TeamID)
  
  prob <- pred %>% 
    filter(ID %in% c(paste0("2023_",team_id_1,"_",team_id_2),
                     paste0("2023_",team_id_2,"_",team_id_1))) %>% 
    pull(pred)
  
  if(team_id_1 > team_id_2) prob = 1 - prob
  
  sample(c(team_name_1, team_name_2), prob = c(prob, 1 - prob), 1)
}

set.seed(123)

## Greenville Region 1

greenville_1_game_1 <- predict("South Carolina", "Norfolk St")
greenville_1_game_2 <- predict("South Florida", "Marquette")
greenville_1_game_3 <- predict("Oklahoma", "Portland")
greenville_1_game_4 <- predict("UCLA", "CS Sacramento")
greenville_1_game_5 <- predict("Creighton", "Illinois")
greenville_1_game_6 <- predict("Notre Dame", "Southern Utah")
greenville_1_game_7 <- predict("Arizona", "West Virginia")
greenville_1_game_8 <- predict("Maryland", "Holy Cross")

greenville_1_game_9 <- predict(greenville_1_game_1, greenville_1_game_2)
greenville_1_game_10 <- predict(greenville_1_game_3, greenville_1_game_4)
greenville_1_game_11 <- predict(greenville_1_game_5, greenville_1_game_6)
greenville_1_game_12 <- predict(greenville_1_game_7, greenville_1_game_8)

greenville_1_game_13 <- predict(greenville_1_game_9, greenville_1_game_10)
greenville_1_game_14 <- predict(greenville_1_game_11, greenville_1_game_12)

greenville_1_champ <- predict(greenville_1_game_13, greenville_1_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("greenville_1_champ")); break}
  
  print(get(paste0("greenville_1_game_",i)))
  
}


## Seattle Region 1

seattle_1_game_1 <- predict("Stanford", "Seton Hall")
seattle_1_game_2 <- predict("Mississippi", "Gonzaga")
seattle_1_game_3 <- predict("Louisville", "Drake")
seattle_1_game_4 <- predict("Texas", "East Carolina")
seattle_1_game_5 <- predict("Colorado", "MTSU")
seattle_1_game_6 <- predict("Duke", "Iona")
seattle_1_game_7 <- predict("Florida St", "Georgia")
seattle_1_game_8 <- predict("Iowa", "SE Louisiana")

seattle_1_game_9 <- predict(seattle_1_game_1, seattle_1_game_2)
seattle_1_game_10 <- predict(seattle_1_game_3, seattle_1_game_4)
seattle_1_game_11 <- predict(seattle_1_game_5, seattle_1_game_6)
seattle_1_game_12 <- predict(seattle_1_game_7, seattle_1_game_8)

seattle_1_game_13 <- predict(seattle_1_game_9, seattle_1_game_10)
seattle_1_game_14 <- predict(seattle_1_game_11, seattle_1_game_12)

seattle_1_champ <- predict(seattle_1_game_13, seattle_1_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("seattle_1_champ")); break}
  
  print(get(paste0("seattle_1_game_",i)))
  
}

## Greenville Region 2

greenville_2_game_1 <- predict("Indiana", "Monmouth NJ")
greenville_2_game_2 <- predict("Oklahoma St", "Miami FL")
greenville_2_game_3 <- predict("Washington St", "FL Gulf Coast")
greenville_2_game_4 <- predict("Villanova", "Cleveland St")
greenville_2_game_5 <- predict("Michigan", "UNLV")
greenville_2_game_6 <- predict("LSU", "Hawaii")
greenville_2_game_7 <- predict("NC State", "Princeton")
greenville_2_game_8 <- predict("Utah", "Gardner Webb")

greenville_2_game_9 <- predict(greenville_2_game_1, greenville_2_game_2)
greenville_2_game_10 <- predict(greenville_2_game_3, greenville_2_game_4)
greenville_2_game_11 <- predict(greenville_2_game_5, greenville_2_game_6)
greenville_2_game_12 <- predict(greenville_2_game_7, greenville_2_game_8)

greenville_2_game_13 <- predict(greenville_2_game_9, greenville_2_game_10)
greenville_2_game_14 <- predict(greenville_2_game_11, greenville_2_game_12)

greenville_2_champ <- predict(greenville_2_game_13, greenville_2_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("greenville_2_champ")); break}
  
  print(get(paste0("greenville_2_game_",i)))
  
}

## Seattle Region 2

seattle_2_game_1 <- predict("Virginia Tech", "Chattanooga")
seattle_2_game_2 <- predict("USC", "S Dakota St")
seattle_2_game_3 <- predict("Iowa St", "Toledo")
seattle_2_game_4 <- predict("Tennessee", "St Louis")
seattle_2_game_5 <- predict("North Carolina", "Purdue")
seattle_2_game_6 <- predict("Ohio St", "James Madison")
seattle_2_game_7 <- predict("Baylor", "Alabama")
seattle_2_game_8 <- predict("Connecticut", "Vermont")

seattle_2_game_9 <- predict(seattle_2_game_1, seattle_2_game_2)
seattle_2_game_10 <- predict(seattle_2_game_3, seattle_2_game_4)
seattle_2_game_11 <- predict(seattle_2_game_5, seattle_2_game_6)
seattle_2_game_12 <- predict(seattle_2_game_7, seattle_2_game_8)

seattle_2_game_13 <- predict(seattle_2_game_9, seattle_2_game_10)
seattle_2_game_14 <- predict(seattle_2_game_11, seattle_2_game_12)

seattle_2_champ <- predict(seattle_2_game_13, seattle_2_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("seattle_2_champ")); break}
  
  print(get(paste0("seattle_2_game_",i)))
  
}

final_four_1 <- predict(greenville_1_champ, seattle_1_champ)
final_four_2 <- predict(greenville_2_champ, seattle_2_champ)

print(final_four_1)
print(final_four_2)

champ <- predict(final_four_1, final_four_2)

print(champ)

total_points <- read_csv("data/WNCAATourneyCompactResults.csv") %>% 
  group_by(Season) %>% 
  summarise(champ_score = last(WScore) + last(LScore)) %>% 
  ungroup() %>% 
  summarise(pts = round(mean(champ_score))) %>% 
  pull(pts)

print(total_points)
