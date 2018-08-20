library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)

setwd("E://Training data//Tableau - Fratal Analytics Workshop//Batch 3 - Mumbai - 11th and 12th June 2018//Case study//Data Set for Case study")

ball_by_ball = read_excel("Ball_by_Ball.xlsx")
match = read_excel("Match.xlsx")
player = read_excel("Player.xlsx")
player_match = read_excel("Player_Match.xlsx")
Season = read_excel("Season.xlsx")
Team = read_excel("Team.xlsx")

# Lets merge few data set

ball_by_ball_New = merge(x = ball_by_ball, y = player, by.x = "Striker_Id", 
                         by.y = "Player_Id", all.x = TRUE)

ball_by_ball_New1 = merge(x = ball_by_ball_New, y = Season, by.x = "Session_Id",
                          by.y = "Season_Id", all.x = TRUE)

ball_by_ball_Final = merge(x = ball_by_ball_New1, y = Team, by.x = "Team_Batting_Id", 
                           by.y = "Team_Id" , all.x = TRUE)

## Header of the dashboard

header = dashboardHeader(title = "IPL Dashboard")

## Sidebar
sidebar = dashboardSidebar(selectInput(inputId = "Year", label = "Select the Year", 
                                       choices = c("All", unique(ball_by_ball_Final$Season_Year)), selected = "All"),
                           sidebarMenu(
                             menuItem("Dashboard", tabName = "KPI", icon = icon("Dashboard")),
                             menuItem("Data", tabName = "Row_Data"),
                             menuItem("Visit Us", href = "http://google.com")))

## Body for the dashboard

body = dashboardBody(
  tabItems(
    tabItem(tabName = "KPI",
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")),
            
            # Fluid row for two charts
            
            fluidRow(
              box(title = "Top 10 batsman", width = 6, collapsible = TRUE, plotOutput("TOP10")),
              box(title = "Top 10 6-hitter", width = 6, collapsible = TRUE, plotOutput("TOP6s")))),
    
    # this is for new tab
    tabItem(tabName = "Row_Data",
            fluidRow(
              box(title = "Row Data", width = 12, collapsible = TRUE, dataTableOutput("RD"))))
  )
)


# Lets create a UI

UI = dashboardPage(header = header, sidebar = sidebar, body = body)


############################################################################


server = function(input, output) {
  
  output$value1 = renderValueBox({
    
    data <- {
      if(input$Year == "All"){
        data1 = ball_by_ball_Final
      } else{
        data1 = filter(ball_by_ball_Final, Season_Year == input$Year)
      }
      data1
    }
    valueBox(length(unique(data$Match_Id)), 'Number of Matches')
  })
 
  # No. of 6s
  
  output$value2 = renderValueBox({
    
    data <- {
      if(input$Year == "All"){
        data1 = filter(ball_by_ball_Final, Batsman_Scored == "6")
      } else{
        data1 = filter(ball_by_ball_Final, Season_Year == input$Year & Batsman_Scored == "6")
      }
      data1
    }
    Most_six = data %>% group_by(Team_Short_Code) %>% summarise(Six = n())
    Team_with_Most_six = Most_six %>% filter(Six == max(Six))
    
    valueBox(sum(Most_six$Six), paste("Team with Max 6s:", Team_with_Most_six$Team_Short_Code))
    
  })
  
  # No. of 50s
  
  output$value3 = renderValueBox({
    
    data <- {
      if(input$Year == "All") {
        data1 = ball_by_ball_Final
      }else {
        data1 = filter(ball_by_ball_Final, Season_Year == input$Year)
      }
      data1
    }
    
    Most_50s = data %>% group_by(Match_Id, Player_Name) %>% 
      summarise(Runs = sum(as.numeric(Batsman_Scored)))
    
    Most_50s$Half_Cent = ifelse(Most_50s$Runs > 49 & Most_50s$Runs < 100, "Half", "NA")
    
    Most_50_Final = Most_50s %>% filter(Half_Cent == "Half") %>% group_by(Player_Name) %>%
      summarise(Count_Of_50s = n())
    
    Player_with_Max_50s = Most_50_Final %>% filter(Count_Of_50s == max(Count_Of_50s))
    
    valueBox(sum(Most_50_Final$Count_Of_50s), paste("Max 50s by:", Player_with_Max_50s$Player_Name))
    
  })
  
  # Top 10 Batsman based on Total Run
  output$TOP10 = renderPlot({
    data <- {
      if(input$Year == "All"){
        data1 = ball_by_ball_Final
      }else {
        data1 = fliter(ball_by_ball_Final, Season_Year == input$Year)
      }
      data1
    }
    Top_10 = data %>% group_by(Player_Name) %>% 
      summarise(Total_Runs = sum(as.numeric(Batsman_Scored))) %>%
      arrange(-Total_Runs)%>% head(10)
    
    ggplot(Top_10, aes(x = reorder(Player_Name, -Total_Runs), y = Total_Runs)) + 
      geom_bar(stat = "Identity", width = .5, fill = "#4CA64C") + theme_bw() + 
      geom_text(aes(label = Total_Runs), position = position_dodge(width = .9), vjust = -0.25) + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(y="Runs", x=" ", caption = "Data Source: data.world") + 
      theme(axis.text.y = element_blank(), axis.ticks.y =element_blank())
      
    
    
  })
  
 
  
   
}














## Create my APP

shinyApp(UI, server)




















