library(dplyr)
library(readr)
library(devtools)

add_z_to_play <- function(a){
  
  g <- 10.725 #yards per second per second
  just_football <- a %>% subset(displayName == "ball")
  allplayers <- a 
  allplayers_passforward <- allplayers %>% subset(event == "pass_forward")
  pass_forward_frame = min(allplayers_passforward$frame)
  allplayers_passarrived <- allplayers %>% subset(event == "pass_arrived")
  pass_arrived_frame = min(allplayers_passarrived$frame)
  only_players <- a %>% subset(displayName != 'ball')
  only_players$seconds_since_throw = (only_players$frame - pass_forward_frame)/10
  only_players$z = 0
  only_players$z1 = 0
  only_players$z2 = 0
  
  specific <- just_football
  specific$seconds_since_throw = (specific$frame- pass_forward_frame)/10
  start_x <- specific %>% subset(frame == pass_forward_frame) %>% pull(x)
  start_y <- specific %>% subset(frame == pass_forward_frame)  %>% pull(y)
  end_x <- specific %>% subset(frame == pass_arrived_frame) %>% pull(x)
  end_y <- specific %>% subset(frame == pass_arrived_frame)  %>% pull(y)
  d <- sqrt((start_x - end_x)^2 + (start_y - end_y)^2) #yards
  vxy = d / ((pass_arrived_frame - pass_forward_frame)/10) # 10 frames a second. vxy in yards per second
  bigT = specific %>% subset(frame == pass_arrived_frame)  %>% pull(seconds_since_throw)
  vz = (bigT*g)/2
  vz1 = (0.5+0.5*g*bigT*bigT)/bigT
  vz2 = (-0.5+0.5*g*bigT*bigT)/bigT
  
  specific$z = 2 + vz*specific$seconds_since_throw - 0.5*g*(specific$seconds_since_throw^2)
  specific$z1 = 1.5 + vz1*specific$seconds_since_throw - 0.5*g*(specific$seconds_since_throw^2)
  specific$z2 = 2.5 + vz2*specific$seconds_since_throw - 0.5*g*(specific$seconds_since_throw^2)
  specific$z[specific$frame > pass_arrived_frame] = 0
  specific$z[specific$frame < pass_forward_frame] = 0
  specific$z1[specific$frame > pass_arrived_frame] = 0
  specific$z1[specific$frame < pass_forward_frame] = 0
  specific$z2[specific$frame > pass_arrived_frame] = 0
  specific$z2[specific$frame < pass_forward_frame] = 0
  totaloutput <- rbind(only_players,specific)
  return(totaloutput)
}





