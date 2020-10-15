library(dplyr)
library(readr)
library(devtools)
source("add_z.R")

w1 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week1.csv")
w2 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week2.csv")
w3 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week3.csv")
w4 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week4.csv")
w5 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week5.csv")
w6 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week6.csv")
w7 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week7.csv")
w8 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week8.csv")
w9 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week9.csv")
w10 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week10.csv")
w11 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week11.csv")
w12 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week12.csv")
w13 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week13.csv")
w14 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week14.csv")
w15 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week15.csv")
w16 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week16.csv")
w17 <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/week17.csv")

all_tracking <- do.call("rbind", list(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17))





passing_plays <- read_csv("/Users/Rishav/Downloads/nfl-big-data-bowl-2021/plays_with_passer.csv")
print(unique(passing_plays$playType))
passing_plays <- passing_plays %>% subset(playType == "play_type_pass")
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
print(nrow(passing_plays))

doWork <- function(passing_plays,p, i) {
  play <- p
  
  play_file_name <- paste(
    "/Users/Rishav/Downloads/nfl-big-data-bowl-2021/addedZ/",
    play$gameId, "_",
    play$playId, "_withZ.csv",
    sep=""
  )
  
  a <- all_tracking %>% subset(playId == play$playId & gameId == play$gameId)
  result_list <- add_z_to_play_with_velocity_big_data(a)
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
  #write.csv(appended_df, play_file_name)
  return(passing_plays)
}

n <- nrow(passing_plays)
for(i in 1:n){
  print(i)
  try(
    passing_plays <- doWork(passing_plays, passing_plays[i,],i)
  )
  
}

write.csv(passing_plays, "/Users/Rishav/Downloads/nfl-big-data-bowl-2021/plays_with_passer_expanded_with_z.csv")


