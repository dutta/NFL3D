library(ggplot2)
library(gg3D)
library(gganimate)
library(dplyr)
library(plotly)
library(Matrix)
library(rlist)

three_d_animate <- function(a) {
  
  fig <-  a %>% plot_ly(width = 1000, height = 650) 
  fig <- fig %>% add_trace(
      x = ~x,
      y = ~y,
      z = ~z+0.5,
      frame = ~frame,
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
  print('test')
  
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
  print('test')
  
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

