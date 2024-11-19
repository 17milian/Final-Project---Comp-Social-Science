#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(usmap)
library(ggplot2)
library(dplyr)
library(tidycensus)
library(highcharter)

state_data <- read.csv("states_FI_graph.csv")
states_F <- read.csv("../FI_State.csv")
colnames(states_F)[2] <- "state"
county_data <- read.csv("counties_FI_graph.csv")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #options menu
      selectInput(inputId = "region_type",choices = c("states","counties"), 
      label = "Choose a region type"),
      selectInput(inputId = "year", choices = c(as.character(2009:2022)), label = "Select a year")
    ),
    mainPanel(
      "Map", plotOutput("usplot", height = 600)
    )
  )
)
#as.character(2009:2022)
# builds the map
getUsMap <- function(level, year) {
  d <- state_data
  title <- "US States"
  if(level == "counties"){
    d <- county_data
    title <- "US Counties"
  }
  plot_usmap(data = d, values = paste("X",year, sep = ""), regions = level) + 
    labs(title = title,
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue")) 
}
server <- function(input, output){
  output$usplot = renderPlot(getUsMap(input$region_type,input$year))
}
shinyApp(ui = ui,server = server)