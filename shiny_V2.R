library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)

ODI = read_excel("E://Training data//DV//DV for Batch 3//Data Set//odi-batting-analysis.xlsx")

# Dashboard header carrying the title of the dashboard
header = dashboardHeader(title = "ODI Dashboard")  

# Sidebar content of the dashboard
sidebar = dashboardSidebar(selectInput(inputId = "Country", label = "Select the Country", choices = as.character(unique(ODI$Country))),
                           selectInput(inputId = "opponent", label = "Select the opponent", choices = c("All", as.character(unique(ODI$Versus))), selected = "All"),
                           sidebarMenu(
                             menuItem("Dashboard", tabName = "KPI", icon = icon("dashboard")),
                              #menuSubItem("Summary", tabName = "KPI"),
                             menuItem("Row Data", tabName = "Row_Data", icon = icon("data")),
                             menuItem("Visit-us", icon = icon("send",lib='glyphicon'), href = "http://www.bcci.tv/")
                           )
)

# To align the elements, one by one, we define them inside fluidRow().

body = dashboardBody(
  tabItems(
    tabItem(tabName = "KPI", 
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")),
            fluidRow( 
              box(title = "Ground wise matches", width = 12, status = "primary", solidHeader = TRUE, 
                  collapsible = TRUE, plotOutput("GWM")))),
    tabItem(tabName = "Row_Data",
            fluidRow(
              box(title = "Row Data" , width = 12, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, dataTableOutput("RW"))))
              
))


#completing the ui part with dashboardPage
ui = dashboardPage(header, sidebar, body, skin='red')

#################################################

# create the server functions for the dashboard  
server = function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  
  #creating the valueBoxOutput content
  output$value1 = renderValueBox({
    data <- {
      if (input$opponent == 'All') {
        data1 <- filter(ODI, Country == input$Country)
      } else {
        data1 <- filter(ODI, Country == input$Country & Versus == input$opponent)
      }
      data1
    }
    highest.avg.strick = data %>% group_by(Player) %>% summarise(value = mean(ScoreRate, na.rm = T)) %>% filter(value==max(value))
    valueBox(highest.avg.strick$value, paste('Top Player:',highest.avg.strick$Player), color = "purple")  
  })
  
  output$value2 = renderValueBox({
    data <- {
      if (input$opponent == 'All') {
        data1 <- filter(ODI, Country == input$Country)
      } else {
        data1 <- filter(ODI, Country == input$Country & Versus == input$opponent)
      }
      data1
    }
    total.runs = data %>% summarise(runs = sum(Runs, na.rm = T))
    valueBox(total.runs, 'Total Runs', color = "green")  
  })
  
  output$value3 = renderValueBox({
    data <- {
      if (input$opponent == 'All') {
        data1 <- filter(ODI, Country == input$Country)
      } else {
        data1 <- filter(ODI, Country == input$Country & Versus == input$opponent)
      }
      data1
    }
    No.of.cent = data %>% filter(Runs>99) %>% group_by(Player) %>% summarise(value = n()) %>% filter(value==max(value))
    valueBox(No.of.cent$value, paste('Top Player:',No.of.cent$Player), color = "yellow")   
  })
  
  #creating the plotOutput content
  output$GWM = renderPlot({
    data <- {
      if (input$opponent == 'All') {
        data1 <- filter(ODI, Country == input$Country)
      } else {
        data1 <- filter(ODI, Country == input$Country & Versus == input$opponent)
      }
      data1
    }
    Top_10 = data %>% group_by(Ground) %>% summarise(No.of.Match = n()) %>% arrange(-No.of.Match) %>% head(10)
    ggplot(data = Top_10, aes(x=Ground, y= No.of.Match)) + 
      geom_bar(stat = "Identity") + ylab("Number of Matches") + xlab("Ground") + 
      theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Ground wise matches") 
  })
  
  # Creating a table output
  output$RW = renderDataTable({
    data <- {
      if (input$opponent == 'All') {
        data1 <- filter(ODI, Country == input$Country)
      } else {
        data1 <- filter(ODI, Country == input$Country & Versus == input$opponent)
      }
      data1
    }
    
    d = data %>% group_by(Versus) %>% summarise(Number.of.Match = n_distinct(MatchDate), 
                                                Total.Runs = sum(Runs, na.rm = TRUE), 
                                                Average.Score.Rate = round(mean(ScoreRate, na.rm = TRUE),1))
    DT::datatable(d, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
  })
  
}

#run/call the shiny app

shinyApp(ui, server)
