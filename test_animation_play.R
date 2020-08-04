source("animate_play.R")
source("add_z.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")

play_file_name <- paste(
  "https://raw.githubusercontent.com/dutta/parabolizR/master/data/",
  2018, "_",
  "CAR", "_",
  2018121700, "_",
  610, "_withZ.csv",
  sep=""
)

play_data <- read_csv(play_file_name)
sub <- play_data%>% subset(displayName == "ball")
added_z <- add_z_to_play(play_data)
fig <- three_d_animate_static(added_z) #if you want a frame by frame animation call three_d_animate(added_z)
fig


