source("animate_play.R")
source("add_z.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")

play_data <- fetch_play_data(playKey_ = 248)
added_z <- add_z_to_play(play_data)
fig <- three_d_animate_static(added_z) #if you want a frame by frame animation call three_d_animate(added_z)
fig


