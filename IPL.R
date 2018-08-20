library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)

# Importing data
setwd("E://Training data//Tableau - Fratal Analytics Workshop//Batch 3 - Mumbai - 11th and 12th June 2018//Case study//Data Set for Case study")

ball_by_ball = read_excel("Ball_by_Ball.xlsx")
match = read_excel("Match.xlsx")
player = read_excel("Player.xlsx")
player_match = read_excel("Player_match.xlsx")
Season = read_excel("Season.xlsx")
Team = read_excel("Team.xlsx")

# Lets merge some of them

ball_by_ball_New = merge(x = ball_by_ball, y = player, by.x = "Striker_Id", by.y = "Player_Id", all.x = TRUE) 

ball_by_ball_New1 = merge(x = ball_by_ball_New, y = Season, by.x = "Session_Id", by.y = "Season_Id", all.x = TRUE)

ball_by_ball_final = merge(x = ball_by_ball_New1, y = Team, by.x = "Team_Batting_Id", by.y = "Team_Id", all.x = TRUE)

# Header of my dashboard

header = dashboardHeader(title = "IPL Dashboard")

# Side bars for my dashboard

sidebar = dashboardSidebar(selectInput(inputId = "Year", label = "Select the Year", 
                                       choices = c("All", unique(ball_by_ball_final$Season_Year)), selected = "All"),
                           sidebarMenu(
                             menuItem("Dashboard", tabName = "KPI", icon = icon("dashboard")),
                             menuItem("Data", tabName = "Row_data"),
                             menuItem("Visit-us", icon = icon("send",lib='glyphicon'), href = "http://www.bcci.tv/")
                           )
)

# Lets define the layout

body = dashboardBody(
  tabItems(
    tabItem(tabName = "KPI",
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")),
            
            fluidRow(
              box(title = "Top 10 Batsman", width = 6, solidHeader = TRUE, collapsible = TRUE, plotOutput("TOP10")),
              box(title = "Top 10 Batsman based on 6s", width = 6, solidHeader = TRUE, collapsible = TRUE, plotOutput("6s")))),
    
    tabItem(tabName = "Row_data",
            fluidRow(
              box(title = "Data Table", width = 12, solidHeader = TRUE, collapsible = TRUE, dataTableOutput("RW"))))
    
))
  
#completing the ui part with dashboardPage
ui = dashboardPage(header, sidebar, body, skin='red')

################################################################

server = function(input, output) {
  
  #creating the valueBoxOutput content
  output$value1 = renderValueBox({
    data <- {
      if (input$Year == 'All') {
        data1 <- ball_by_ball_final
      } else {
        data1 <- filter(ball_by_ball_final, Season_Year == input$Year)
      }
      data1
    }
    
    valueBox(length(unique(data$Match_Id)), 'Number of Matches') 
  })
  
  output$value2 = renderValueBox({
    data <- {
      if (input$Year == 'All') {
        data1 <- filter(ball_by_ball_final, Batsman_Scored == "6")
      } else {
        data1 <- filter(ball_by_ball_final, Season_Year == input$Year & Batsman_Scored == "6")
      }
      data1
    }
    Most_Six = data %>% group_by(Team_Short_Code) %>% summarise(Six = n()) %>% filter(Six == max(Six))
    valueBox(sum(Most_Six$Six), paste('Team with Most Sixes:',Most_Six$Team_Short_Code), color = "purple") 
  })
  
  output$value3 = renderValueBox({
    data <- {
      if (input$Year == 'All') {
        data1 <- ball_by_ball_final
      } else {
        data1 <- filter(ball_by_ball_final, Season_Year == input$Year)
      }
      data1
    }
    Most_50s = data %>% group_by(Match_Id, Player_Name) %>% summarise(Runs = sum(as.numeric(Batsman_Scored)))
    
    Most_50s$Status = ifelse(Most_50s$Runs > 49 & Most_50s$Runs < 100, "Half", "NA")
    
    Most_50s_Final = Most_50s %>% filter(Status == "Half") %>% group_by(Player_Name) %>% summarise(Half_Cent = n()) %>% 
            filter(Half_Cent == max(Half_Cent))
    
    valueBox(sum(Most_50s_Final$Half_Cent), paste('Most Sixes by:',Most_50s_Final$Player_Name), color = "green") 
  })
  
  
}
  





#############################################################
#run/call the shiny app

shinyApp(ui, server)
  
  
  
  
