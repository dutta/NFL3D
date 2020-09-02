library(ggplot2)
library(dplyr)
library(plotly)
library(Matrix)
require(rlist)

three_d_animate <- function(a) {
  a$color = ifelse(a$teamAbbr == "ball", "brown", a$teamAbbr)
  fig <-  a %>% plot_ly(width = 1000, height = 650) %>% animation_opts(
    1000, easing = "elastic", redraw = TRUE
  )
  fig <- fig %>% add_trace(
      x = ~x,
      y = ~y,
      z = ~z+0.5,
      frame = ~frame,
      type = 'scatter3d',
      text = ~jerseyNumber,
      mode = 'markers+text',
      showlegend = F,
      color = ~color,
      textposition = 'center',
      textfont = list(size= 10, color= "black"),
      connectgaps=FALSE
    )
  
  field <- vector("list", 11) 
  for (i in 1:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = -0.01)
    }
    field[[i]] <- do.call(rbind, row)
  }
  field <- do.call(rbind, field)
  field <- as.data.frame(field)
  
  
  endzone1 <- vector("list", 11) 
  for (i in 1:11) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = 0)
    }
    endzone1[[i]] <- do.call(rbind, row)
  }
  endzone1 <- do.call(rbind, endzone1)
  endzone1 <- as.data.frame(endzone1)
  
  
  endzone2 <- vector("list", 11) 
  for (i in 110:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i,y = j-1, z = 0)
    }
    endzone2[[i]] <- do.call(rbind, row)
  }
  endzone2 <- do.call(rbind, endzone2)
  endzone2 <- as.data.frame(endzone2)
  
  fig <- fig %>% layout( 
    scene = list(aspectmode = "manual", aspectratio = list(x=2, y=1, z=0.5),
                 xaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,125),title = '',visible = FALSE),
                 yaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,60),title = '',visible = FALSE),
                 zaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-1,20),title = '',visible = TRUE)))
  
  fig <- fig %>% add_trace(type = "mesh3d", x = field$x, y = field$y, z = field$z,showlegend = F,showscale = FALSE,intensity =field$x,
                           colorscale = list(c(0, 1), c("green", "green")))
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone1$x, y = endzone1$y, z = endzone1$z, intensity = endzone1$z, colorscale = list(c(0, 1), c("blue", "blue")),showlegend = F,showscale = FALSE)
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("red", "red")),showlegend = F,showscale = FALSE)
  #yardlines
  for(start in seq(10,110,5)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in 1:55){
        row[[j]] <- data.frame(x = i,y = j-1, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    # print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  #hashmarks
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(22.5,23.5,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(28.75,29.75,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  
  return(fig)
}
three_d_animate_static <- function(a) {
  
  fig <-  a %>% plot_ly(width = 1000, height = 650) 
  fig <- fig %>% add_trace(
    x = ~x,
    y = ~y,
    z = ~z+0.5,
    #frame = ~frame,
    type = 'scatter3d',
    mode = 'markers',
    showlegend = F,
    color = ~teamAbbr,
    connectgaps=FALSE
  )
  
  field <- vector("list", 11) 
  for (i in 1:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = -0.01)
    }
    field[[i]] <- do.call(rbind, row)
  }
  field <- do.call(rbind, field)
  field <- as.data.frame(field)
  
  
  endzone1 <- vector("list", 11) 
  for (i in 1:11) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = 0)
    }
    endzone1[[i]] <- do.call(rbind, row)
  }
  endzone1 <- do.call(rbind, endzone1)
  endzone1 <- as.data.frame(endzone1)
  
  
  endzone2 <- vector("list", 11) 
  for (i in 110:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i,y = j-1, z = 0)
    }
    endzone2[[i]] <- do.call(rbind, row)
  }
  endzone2 <- do.call(rbind, endzone2)
  endzone2 <- as.data.frame(endzone2)
  
  fig <- fig %>% layout( 
    scene = list(aspectmode = "manual", aspectratio = list(x=2, y=1, z=0.5),
                 xaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,125),title = '',visible = FALSE),
                 yaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,60),title = '',visible = FALSE),
                 zaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-1,20),title = '',visible = TRUE)))
  
  fig <- fig %>% add_trace(type = "mesh3d", x = field$x, y = field$y, z = field$z,showlegend = F,showscale = FALSE,intensity =field$x,
                           colorscale = list(c(0, 1), c("green", "green")))
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone1$x, y = endzone1$y, z = endzone1$z, intensity = endzone1$z, colorscale = list(c(0, 1), c("blue", "blue")),showlegend = F,showscale = FALSE)
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("red", "red")),showlegend = F,showscale = FALSE)
  
  #yardlines
  for(start in seq(10,110,5)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in 1:55){
        row[[j]] <- data.frame(x = i,y = j-1, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    # print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  #hashmarks
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(22.5,23.5,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(28.75,29.75,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  
  return(fig)
}
three_d_all_passes <- function(a) {
  fig <- plot_ly(width = 1000, height = 650) 
  a <- list.sort(a, v)
  for(i in 1:length(a)){

    data <- a[[i]]$df
    fig <- fig %>% add_trace(data = data,
      x = ~x,
      y = ~y,
      z = ~z-.5,
      #frame = ~frame,
      type = 'scatter3d',
      mode = 'lines',
      color = ~velocity,
      name = ~round(velocity*2.05, 2),
      line = list(width = 5),
      connectgaps=FALSE
    )
  }
  fig <-  fig %>% layout(showlegend=TRUE)
  
  field <- vector("list", 11) 
  for (i in 1:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = -0.01)
    }
    field[[i]] <- do.call(rbind, row)
  }
  field <- do.call(rbind, field)
  field <- as.data.frame(field)
  
  
  endzone1 <- vector("list", 11) 
  for (i in 1:11) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i-1,y = j-1, z = 0)
    }
    endzone1[[i]] <- do.call(rbind, row)
  }
  endzone1 <- do.call(rbind, endzone1)
  endzone1 <- as.data.frame(endzone1)
  
  
  endzone2 <- vector("list", 11) 
  for (i in 110:121) {
    row <- vector("list", 53)
    for(j in 1:55){
      row[[j]] <- data.frame(x = i,y = j-1, z = 0)
    }
    endzone2[[i]] <- do.call(rbind, row)
  }
  endzone2 <- do.call(rbind, endzone2)
  endzone2 <- as.data.frame(endzone2)
  
  fig <- fig %>% layout( 
    scene = list(aspectmode = "manual", aspectratio = list(x=2, y=1, z=0.5),
                 xaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,125),title = '',visible = FALSE),
                 yaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-5,60),title = '',visible = FALSE),
                 zaxis = list(dtick = 1, tick0 = 0, tickmode = "linear", range = c(-1,20),title = '',visible = TRUE)))
  
  fig <- fig %>% add_trace(type = "mesh3d", x = field$x, y = field$y, z = field$z,showlegend = F,showscale = FALSE,intensity =field$x,
                           colorscale = list(c(0, 1), c("green", "green")))
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone1$x, y = endzone1$y, z = endzone1$z, intensity = endzone1$z, colorscale = list(c(0, 1), c("blue", "blue")),showlegend = F,showscale = FALSE)
  fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("red", "red")),showlegend = F,showscale = FALSE)
  
  #yardlines
  for(start in seq(10,110,5)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in 1:55){
        row[[j]] <- data.frame(x = i,y = j-1, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    # print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  #hashmarks
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(22.5,23.5,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  for(start in seq(10,110,1)){
    endzone2 <- vector("list", 2) 
    
    for (i in seq(start-0.1,start+0.1,0.1)) {
      #print(start)
      row <- vector("list", 53)
      for(j in seq(28.75,29.75,0.5)){
        row[[j]] <- data.frame(x = i,y = j, z = 0.1)
      }
      endzone2[[i]] <- do.call(rbind, row)
    }
    #print('test')
    endzone2 <- do.call(rbind, endzone2)
    endzone2 <- as.data.frame(endzone2)
    #print(endzone2)
    fig <- fig %>% add_trace(type = "mesh3d",x = endzone2$x, y = endzone2$y, z = endzone2$z, intensity = endzone2$z, colorscale = list(c(0, 1), c("white", "white")),showlegend = F,showscale = FALSE)
    
  }
  
  #fig <- fig %>% layout(scene = list(title = "TEST", camera = list(x = -1.25, y = 1.25, z = 1.25),  up = list(x = 0, y = 0, z = 5)))
  return(fig)
}

get_football_arc <- function(data, gameId, playId,season, team, throw,catch,direction,los, vel){
  play_file_name <- paste(
    "https://raw.githubusercontent.com/dutta/NFL3D/master/data/ngs/",
    season, "_",
    team, "_",
    gameId, "_",
    playId, "_withZ.csv",
    sep=""
  )
  play_data <- read_csv(play_file_name)
  sub <- play_data%>% subset(displayName == "ball")
  sub <- sub %>% subset(frame >= throw & frame <= catch)
  launch_point <- sub %>% subset(frame == throw) %>% pull(x)
  if(launch_point > 90){
    sub$x <-  sub$x  - (launch_point - 90)
  } else{
    sub$x <- sub$x  + (90- launch_point) 
  }
  if(direction == "right"){
    sub$x <- 90 - (sub$x - 90)
  }
  sub$velocity <- vel
  sub <- sub %>% select(x,y,z,playId,gameId,velocity) %>% mutate_if(is.numeric, round, digits = 2)
  return(sub)
}

get_all_arcs_for_passer <- function(p){
  data <-  read_csv("https://raw.githubusercontent.com/dutta/NFL3D/master/data/ngs_passing_play_index_expanded_full.csv")
  sub_list <- data %>% subset(passer == p)
  first <- get_football_arc(data, sub_list[1,]$gameId,sub_list[1,]$playId,sub_list[1,]$season,sub_list[1,]$teamAbbr, sub_list[1,]$pass_forward_frame, 
                            sub_list[1,]$pass_arrived_frame,sub_list[1,]$play.playDirection,sub_list[1,]$play.absoluteYardlineNumber, sub_list[1,]$v)
  l <- vector("list", nrow(sub_list))
  l[[1]] <- list("df" = first, v = sub_list[1,]$v)
  for(i in 2:nrow(sub_list)){
    temp <- get_football_arc(data, sub_list[i,]$gameId,sub_list[i,]$playId,sub_list[i,]$season,sub_list[i,]$teamAbbr, sub_list[i,]$pass_forward_frame, 
                             sub_list[i,]$pass_arrived_frame,sub_list[i,]$play.playDirection,sub_list[i,]$play.absoluteYardlineNumber, sub_list[i,]$v)
    l[[i]] <- list("df" = temp, v = sub_list[i,]$v)
  }
  return(l)
  
}

get_play_animation_data <- function(playId, gameId, season, team){
  play_file_name <- paste(
    "https://raw.githubusercontent.com/dutta/NFL3D/master/data/ngs/",
    season, "_",
    team, "_",
    gameId, "_",
    playId, "_withZ.csv",
    sep=""
  )
  data <-  read_csv(play_file_name)
  return(data)
}

