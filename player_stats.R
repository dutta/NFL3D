library(dplyr)
library(readr)
source("add_z.R")
player_game_data <- read_csv("/Users/Rishav/Documents/nflScrapR/parabolizR/data/ngs_passing_play_index_expanded_full.csv")

by_passer<- player_game_data %>% group_by(passer) %>% summarise(mean_launch_angle = mean(launch_angle), num_passes = n()) %>% 
  arrange(desc(mean_launch_angle)) %>% filter(mean_launch_angle < 45) %>% filter(num_passes > 3)
by_speed<- player_game_data %>% group_by(passer) %>% summarise(mean_velocity = mean(v), num_passes = n()) %>% arrange(desc(mean_velocity)) %>% filter(num_passes > 3)

by_passer
by_speed

