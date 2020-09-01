library(shiny)
source("https://raw.githubusercontent.com/dutta/NFL3D/master/animate_play.R")
#source("../animate_play.R")
library(dplyr)
library(readr)
library(plotly)
library(shinythemes)
library(DT)
require(rlist)

df <- read_csv("https://raw.githubusercontent.com/dutta/NFL3D/master/data/ngs_passing_play_index_expanded_full.csv")
players <- df %>% group_by(passer) %>% filter(launch_angle <= 45)
grouped <- df %>% group_by(passer) %>% summarise("Number Passes" = n(), "Mean Launch Angle (d)" = mean(launch_angle), "Mean Launch Velocity (mph)" = mean(v)*2.05) %>% mutate_if(is.numeric, round, digits = 2)
plays <- df %>% select(play.playDescription, playId, gameId)
#print(grouped)
arc_data <- get_all_arcs_for_passer("R.Wilson")
play_data <- get_play_animation_data(3187,2020020200,2019,"SF")

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  titlePanel(""),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(condition = "input.tabselected == 1",  div("Select a A Player", style = "height:700px; overflow: scroll;",  dataTableOutput("x1"))),
      conditionalPanel(condition = "input.tabselected == 2",  div("Select a A Play", style = "height:700px; overflow: scroll;",dataTableOutput("x2")))
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Player Plot", h3(textOutput("t1")), plotlyOutput(outputId = "distPlot"), 
                 HTML("<br><br><br><br><br><br><br><br><br><br><br>"),
                 plotOutput(outputId = "anglePlot"), plotOutput(outputId = "velPlot"), value = 1),
        tabPanel("Play Plot",  h3(textOutput("t2")), plotlyOutput(outputId = "playPlot"), value = 2), id = "tabselected"
      )
      
      
    )
  )
)

""
server <- function(input, output) {
  output$x1 = renderDataTable(grouped, server = TRUE, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, pageLength = 70))
  output$x2 = renderDataTable(plays, server = TRUE, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, pageLength = 25), 
                              colnames = c("Description", "playId","gameId"))
  
  output$t1 <- renderText({"R.Wilson NGS Passing Chart in 3D"})
  output$t2 <- renderText({"(6:17) P.Mahomes pass short right to T.Kelce for 1 yard, TOUCHDOWN."})
    
  output$distPlot <- renderPlotly({
    s <- input$x1_rows_selected
    print(s)
    if(is.null(s)){
      output$t1 <- renderText({"R.Wilson NGS Passing Chart in 3D"})
      three_d_all_passes(arc_data)
    } else{
      dataim <- grouped[s,]
      print(dataim$passer)
      output$t1 <- renderText({paste(dataim$passer, " NGS Passing Chart in 3D", sep="")})
      arc_data <- get_all_arcs_for_passer(dataim$passer)
      three_d_all_passes(arc_data)
    }
  })
  
  output$anglePlot <- renderPlot({
    s <- input$x1_rows_selected
    print(s)
    if(is.null(s)){
      output$t1 <- renderText({"R.Wilson NGS Passing Chart in 3D"})
      temp <- players %>% subset(passer == "R.Wilson")
      ggplot(temp, aes(x=launch_angle)) + geom_density(alpha=.2, fill="#FF6666") + 
        geom_vline(aes(xintercept=mean(launch_angle, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Distribution of Launch Angle (degrees)")+ xlab("Launch Angle")
    } else{
      dataim <- grouped[s,]
      print(dataim$passer)
      output$t1 <- renderText({paste(dataim$passer, " NGS Passing Chart in 3D", sep="")})
      temp <- players %>% subset(passer == dataim$passer)
      ggplot(temp, aes(x=launch_angle)) + geom_density(alpha=.2, fill="#FF6666") + 
        geom_vline(aes(xintercept=mean(launch_angle, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Distribution of Launch Angle (degrees)")+ xlab("Launch Angle")
    }
  })
  
  output$velPlot <- renderPlot({
    s <- input$x1_rows_selected
    print(s)
    if(is.null(s)){
      output$t1 <- renderText({"R.Wilson NGS Passing Chart in 3D"})
      temp <- players %>% subset(passer == "R.Wilson")
      ggplot(temp, aes(x=v*2.05)) + geom_density(alpha=.2, fill="#FF6666") + 
        geom_vline(aes(xintercept=mean(v*2.05, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Distribution of Launch Velocity (mph)") + xlab("Launch Velocity")
    } else{
      dataim <- grouped[s,]
      print(dataim$passer)
      output$t1 <- renderText({paste(dataim$passer, " NGS Passing Chart in 3D", sep="")})
      temp <- players %>% subset(passer == dataim$passer)
      ggplot(temp, aes(x=v*2.05)) + geom_density(alpha=.2, fill="#FF6666") + 
        geom_vline(aes(xintercept=mean(v*2.05, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Distribution of Launch Velocity (mph)")+ xlab("Launch Velocity")
    }
  })
  
  
  
 
  
  output$playPlot <- renderPlotly({
    s <- input$x2_rows_selected
    print(s)
    if(is.null(s)){
      output$t2 <- renderText({"(6:17) P.Mahomes pass short right to T.Kelce for 1 yard, TOUCHDOWN."})
      three_d_animate(play_data)
    } else{
      dataim <- plays[s,]
      subvals <- df %>% subset(playId == dataim$playId & gameId == dataim$gameId)
      print(subvals$season)
      output$t2 <-renderText({subvals$play.playDescription})
      
      arc_data <- get_play_animation_data(subvals$playId,subvals$gameId,subvals$season,subvals$teamAbbr)
      three_d_animate_static(arc_data)
    }
  })
  
  
}

shinyApp(ui = ui, server = server)