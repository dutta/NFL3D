library(dplyr)
library(readr)
library(devtools)
source("add_z.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")

passing_plays <- read_csv("/Users/Rishav/Documents/nflScrapR/parabolizR/data/ngs_passing_play_index_expanded.csv")
passing_plays$v <- 0
passing_plays$vxy <- 0
passing_plays$vz <- 0
passing_plays$launch_angle <- 0
passing_plays$air_time <- 0
passing_plays$z_max <- 0
passing_plays$distance <- 0
passing_plays$start_x <- 0
passing_plays$start_y <- 0
passing_plays$end_x <- 0
passing_plays$end_y <- 0
passing_plays$pass_arrived_frame <- 0
passing_plays$pass_forward_frame <- 0

for(i in 1:nrow(passing_plays)){
  play <- passing_plays[i,]
  print(play)
  play_file <- paste(
    "/Users/Rishav/Documents/nflScrapR/ngs_highlights/play_data/",
    play$season, "_",
    play$teamAbbr, "_",
    play$gameId, "_",
    play$playId, ".tsv",
    sep=""
  )
  play_file_name <- paste(
    "/Users/Rishav/Documents/nflScrapR/parabolizR/data/",
    play$season, "_",
    play$teamAbbr, "_",
    play$gameId, "_",
    play$playId, "_withZ.csv",
    sep=""
  )
  a <- read_tsv(play_file)
  result_list <- add_z_to_play_with_velocity(a)
  passing_plays$v[i] <- result_list$v
  passing_plays$vxy[i] <- result_list$vxy
  passing_plays$vz[i] <- result_list$vz
  passing_plays$launch_angle[i] <- result_list$launch_angle
  passing_plays$air_time[i] <- result_list$air_time
  passing_plays$z_max[i] <- result_list$z_max
  passing_plays$distance[i] <- result_list$distance
  passing_plays$start_x[i] <- result_list$start_x
  passing_plays$start_y[i] <- result_list$start_y
  passing_plays$end_x[i] <- result_list$end_x
  passing_plays$end_y[i] <- result_list$end_y
  passing_plays$pass_arrived_frame[i] <- result_list$pass_arrived_frame
  passing_plays$pass_forward_frame[i] <- result_list$pass_forward_frame
  
  appended_df <- result_list$df
  write.csv(appended_df, play_file_name)
}

write.csv(passing_plays, "/Users/Rishav/Documents/nflScrapR/parabolizR/data/ngs_passing_play_index_expanded_full.csv")


