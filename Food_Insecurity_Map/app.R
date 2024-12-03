#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)

# Load data
state_data <- read.csv("states_FI_graph.csv")
states_F <- read.csv("../FI_State.csv")
colnames(states_F)[2] <- "state"
county_data <- read.csv("counties_FI_graph.csv")

# Preprocess data
state_data_long <- state_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(sub("X", "", year)))

county_data_long <- county_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(sub("X", "", year)))

# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Options menu
      selectInput(inputId = "region_type", choices = c("states", "counties"), label = "Choose a region type"),
      selectInput(inputId = "year", choices = c(2009:2022), label = "Select a year")
    ),
    mainPanel(
      "Interactive Map",
      leafletOutput("leafletMap", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for selected region type and year
  reactive_data <- reactive({
    if (input$region_type == "states") {
      data <- state_data_long %>% filter(year == input$year)
    } else {
      data <- county_data_long %>% filter(year == input$year)
    }
    return(data)
  })
  
  # Render Leaflet map
  output$leafletMap <- renderLeaflet({
    data <- reactive_data()
    
    if (input$region_type == "states") {
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(data = us_states, # Replace with spatial data for US states
                    fillColor = ~colorBin("YlOrRd", domain = data$value, bins = 5)(value),
                    fillOpacity = 0.7,
                    color = "white",
                    weight = 1,
                    popup = ~paste("State:", state, "<br>", "Value:", value)) %>%
        setView(lng = -98.583, lat = 39.833, zoom = 4)
    } else {
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(data = us_counties, # Replace with spatial data for US counties
                    fillColor = ~colorBin("YlOrRd", domain = data$value, bins = 5)(value),
                    fillOpacity = 0.7,
                    color = "white",
                    weight = 0.5,
                    popup = ~paste("County:", county, "<br>", "Value:", value)) %>%
        setView(lng = -98.583, lat = 39.833, zoom = 4)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
