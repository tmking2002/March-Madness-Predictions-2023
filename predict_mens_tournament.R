library(tidyverse)

pred <- read_csv("final_submission_2023.csv")
teams <- read_csv("data/MTeams.csv")

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

set.seed(100)

## South Region

south_game_1 <- predict("Alabama", "SE Missouri St")
south_game_2 <- predict("Maryland", "West Virginia")
south_game_3 <- predict("San Diego St", "Col Charleston")
south_game_4 <- predict("Virginia", "Furman")
south_game_5 <- predict("Creighton", "NC State")
south_game_6 <- predict("Baylor", "UC Santa Barbara")
south_game_7 <- predict("Missouri", "Utah St")
south_game_8 <- predict("Arizona", "Princeton")

south_game_9 <- predict(south_game_1, south_game_2)
south_game_10 <- predict(south_game_3, south_game_4)
south_game_11 <- predict(south_game_5, south_game_6)
south_game_12 <- predict(south_game_7, south_game_8)

south_game_13 <- predict(south_game_9, south_game_10)
south_game_14 <- predict(south_game_11, south_game_12)

south_champ <- predict(south_game_13, south_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("south_champ")); break}
  
  print(get(paste0("south_game_",i)))
  
}


## East Region

east_game_1 <- predict("Purdue", "F Dickinson")
east_game_2 <- predict("Memphis", "FL Atlantic")
east_game_3 <- predict("Duke", "Oral Roberts")
east_game_4 <- predict("Tennessee", "Louisiana")
east_game_5 <- predict("Kentucky", "Providence")
east_game_6 <- predict("Kansas St", "Montana St")
east_game_7 <- predict("Michigan St", "USC")
east_game_8 <- predict("Marquette", "Vermont")

east_game_9 <- predict(east_game_1, east_game_2)
east_game_10 <- predict(east_game_3, east_game_4)
east_game_11 <- predict(east_game_5, east_game_6)
east_game_12 <- predict(east_game_7, east_game_8)

east_game_13 <- predict(east_game_9, east_game_10)
east_game_14 <- predict(east_game_11, east_game_12)

east_champ <- predict(east_game_13, east_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("east_champ")); break}
  
  print(get(paste0("east_game_",i)))
  
}

## Midwest Region

midwest_game_1 <- predict("Houston", "N Kentucky")
midwest_game_2 <- predict("Iowa", "Auburn")
midwest_game_3 <- predict("Miami FL", "Drake")
midwest_game_4 <- predict("Indiana", "Kent")
midwest_game_5 <- predict("Iowa St", "Pittsburgh")
midwest_game_6 <- predict("Xavier", "Kennesaw")
midwest_game_7 <- predict("Texas A&M", "Penn St")
midwest_game_8 <- predict("Texas", "Colgate")

midwest_game_9 <- predict(midwest_game_1, midwest_game_2)
midwest_game_10 <- predict(midwest_game_3, midwest_game_4)
midwest_game_11 <- predict(midwest_game_5, midwest_game_6)
midwest_game_12 <- predict(midwest_game_7, midwest_game_8)

midwest_game_13 <- predict(midwest_game_9, midwest_game_10)
midwest_game_14 <- predict(midwest_game_11, midwest_game_12)

midwest_champ <- predict(midwest_game_13, midwest_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("midwest_champ")); break}
  
  print(get(paste0("midwest_game_",i)))
  
}

## West Region

west_game_1 <- predict("Kansas", "Howard")
west_game_2 <- predict("Arkansas", "Illinois")
west_game_3 <- predict("SMU", "VCU")
west_game_4 <- predict("Connecticut", "Iona")
west_game_5 <- predict("TCU", "Nevada")
west_game_6 <- predict("Gonzaga", "Grand Canyon")
west_game_7 <- predict("Northwestern", "Boise St")
west_game_8 <- predict("UCLA", "UNC Asheville")

west_game_9 <- predict(west_game_1, west_game_2)
west_game_10 <- predict(west_game_3, west_game_4)
west_game_11 <- predict(west_game_5, west_game_6)
west_game_12 <- predict(west_game_7, west_game_8)

west_game_13 <- predict(west_game_9, west_game_10)
west_game_14 <- predict(west_game_11, west_game_12)

west_champ <- predict(west_game_13, west_game_14)

for(i in 1:15){
  
  if(i == 15) {print(get("west_champ")); break}
  
  print(get(paste0("west_game_",i)))
  
}

final_four_1 <- predict(south_champ, east_champ)
final_four_2 <- predict(midwest_champ, west_champ)

print(final_four_1)
print(final_four_2)

champ <- predict(final_four_1, final_four_2)

print(champ)

total_points <- read_csv("data/MNCAATourneyCompactResults.csv") %>% 
  group_by(Season) %>% 
  summarise(champ_score = last(WScore) + last(LScore)) %>% 
  ungroup() %>% 
  summarise(pts = round(mean(champ_score))) %>% 
  pull(pts)

print(total_points)
