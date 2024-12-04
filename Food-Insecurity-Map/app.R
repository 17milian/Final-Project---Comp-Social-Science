#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Read and preprocess the data
state_data <- read.csv("states_FI_graph.csv")
states <- read_sf("cb_2019_us_state_5m.shp")
district_data <- read.csv("FI_Districts.csv")
districts <- read_sf("districts.shp")
if (!all(is.element(state_data$state, states$NAME))) {
  stop("Some states in state_data are not present in the shapefile.")
}

#id = str_remove(id, "^0+")
#merged_districts <- merge(districts, district_data,)


states <- merge(states, state_data, by.x = 'NAME', by.y = 'state', all.x = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Food Insecurity Map"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("Hover over a state to view its details."),
      p("Select a year to view data for that year."),
      selectInput(
        inputId = "selectedYear",
        label = "Select Year",
        choices = setNames(paste0("X", 2009:2022), 2009:2022),  # Display years as plain numbers
        selected = "X2009"  # Default selection
      )
    ),
    mainPanel(
      leafletOutput("foodInsecurityMap", height = 600)
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to update palette and labels based on selected year
  reactiveStates <- reactive({
    yearColumn <- input$selectedYear
    states$fillColor <- colorNumeric('Reds', domain = states[[yearColumn]])(states[[yearColumn]])
    
    states$stateLabels <- paste0(
      "<strong>", states$NAME, "</strong><br/>",
      "Food Insecurity Score (", gsub("X", "", yearColumn), "): ", "<strong>", states[[yearColumn]], "</strong>"
    )
    return(states)
  })
  reactivedistricts <- reactive({
    yearColumn <- input$selectedYear
    
  })
  # Render leaflet map
  output$foodInsecurityMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>% 
      addPolygons(
        data = reactiveStates(),
        color = 'white',
        weight = 1,
        smoothFactor = .3,
        fillOpacity = .75,
        fillColor = ~fillColor,  # Use dynamically updated fillColor
        label = lapply(reactiveStates()$stateLabels, htmltools::HTML),  # Ensure bold text rendering
        labelOptions = labelOptions(
          style = list(color = 'gray30'),
          textsize = '10px'
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = 'red'
        )
      ) %>% 
      addLegend(
        pal = colorNumeric('Reds', domain = reactiveStates()[[input$selectedYear]]), 
        values = reactiveStates()[[input$selectedYear]],
        title = '<small>Food Insecurity Scores</small>',
        position = 'bottomleft'
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
