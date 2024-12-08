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
library(bslib)
library(leaflet)
library(plotly)
library(tidyverse)
library(sf)
library(cdlTools)

# Read and preprocess the data
state_data <- read.csv("states_FI_graph.csv")
states <- read_sf("cb_2019_us_state_5m.shp")
district_data <- read.csv("FI_Districts.csv")
districts <- read_sf("districts.shp")
county_data <- read.csv("counties_FI_graph.csv")
counties <- read_sf("smallCounties.shp")
if (!all(is.element(state_data$state, states$NAME))) {
  stop("Some states in state_data are not present in the shapefile.")
}
for(i in 1:length(district_data[,1])){
  if(as.numeric(district_data[i,1]) < 1000){
    district_data[i,1] <- paste("0", district_data[i,1], sep = "") 
  }
  else{
    district_data[i,1] <- as.character(district_data[i,1])
  }
}
for(i in 1:length(county_data[,19])){
  if(as.numeric(county_data[i,19]) < 10000){
    county_data[i,19] <- paste("0", county_data[i,19], sep = "") 
  }
  else{
    county_data[i,19] <- as.character(county_data[i,19])
  }
}
fi_state <- read.csv("Analysis/Analysis_State.csv")
fi_county <- read.csv("Analysis/Analysis_County.csv")
fi_districts <- read.csv("Analysis/Analysis_District.csv")


states <- merge(states, state_data, by.x = 'NAME', by.y = 'state', all.x = FALSE)
merged_districts <- merge(district_data, districts, by.x = "FIPS", by.y = "GEOID20")
merged_districts <- st_as_sf(merged_districts)
merged_counties <- merge(county_data, counties, by.x = "fips", by.y = "FIPS")
merged_counties <- st_as_sf(merged_counties)
# Define UI
ui <- fluidPage(
  navset_tab(
    nav_panel("Main",
              titlePanel("Food Insecurity Map"),
              sidebarLayout(
                sidebarPanel(
                  h4("Instructions"),
                  p("Hover over a state to view its details."),
                  p("Select a year to view data for that year."),
                  selectInput(
                    inputId = "type",
                    label = "select map type",
                    choices = setNames(c("State","County","District"),c("State","County","District"))
                  ),
                  selectInput(
                    inputId = "selectedYear",
                    label = "Select Year",
                    choices = setNames(paste0("X", 2009:2022), 2009:2022),  # Display years as plain numbers
                    selected = "X2009"  # Default selection
                  ),
                  conditionalPanel(condition = "input.type !== 'State'",
                                   selectInput(
                                     inputId = "selectedState",
                                     label = "Select State",
                                     choices = setNames(
                                       c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17",
                                         "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
                                         "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46",
                                         "47", "48", "49", "50", "51", "53", "54", "55", "56", "72"),
                                       c("Alabama", "Alaska", "Arizona", "Arkansas",
                                         "California", "Colorado", "Connecticut", "Delaware", "District of Columbia",
                                         "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                                         "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                         "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                         "New Jersey", "New Mexico", "New York", "North Carolina","North Dakota",
                                         "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                         "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                                         "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
                                     )
                                   )
                  )
                ),
                mainPanel(
                  leafletOutput("foodInsecurityMap", height = 600)
                )
              )
    ),
    nav_panel("Analysis",
              titlePanel("Food Insecurity Rate Trends"),
              sidebarLayout(
                sidebarPanel(
                  h4("Instructions"),
                  p("Select a geographic level to view further selection."),
                  radioButtons(
                    inputId = "geographic_level",
                    label = "Select Level",
                    choices = c("State", "County", "District"),
                    selected = "State"
                  ),
                  p("Select an area within that level to view trends."),
                  selectizeInput(
                    inputId = "names",
                    label = "Select Area",
                    choices = " ",
                    multiple = TRUE,
                    options = list(maxItems = 2)
                  )
                ),
                mainPanel(
                  plotlyOutput(outputId = "trendLine")
                )
              ))
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
  reactiveCounties <- reactive({
    yearColumn <- input$selectedYear
    merged_counties$fillColor <- colorNumeric('Reds', domain = merged_counties[[yearColumn]])(merged_counties[[yearColumn]])
    
    merged_counties$stateLabels <- paste0(
      "<strong>", merged_counties$name, "</strong><br/>",
      "Food Insecurity Score (", gsub("X", "", yearColumn), "): ", "<strong>", merged_counties[[yearColumn]], "</strong>"
    )
    return(filter(merged_counties, STATE == fips(input$selectedState, to = "ABBREVIATION")))
  })
  reactiveDistricts <- reactive({
    
    yearColumn <- input$selectedYear
    merged_districts$fillColor <- colorNumeric('Reds', domain = merged_districts[[yearColumn]])(merged_districts[[yearColumn]])
    
    merged_districts$stateLabels <- paste0(
      "<strong>", merged_districts$NAME, "</strong><br/>",
      "Food Insecurity Score (", gsub("X", "", yearColumn), "): ", "<strong>", merged_districts[[yearColumn]], "</strong>"
    )
    return(filter(merged_districts,STATEFP20 == input$selectedState))
  })
  
  # Render leaflet map
  createStates <- reactive({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
      addPolygons(
        data = reactiveStates(),
        color = 'white',
        weight = 1,
        smoothFactor = .3,
        fillOpacity = .75,
        fillColor = ~fillColor,  # Use dynamically updated fillColor
        layerId = ~STATEFP,
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
    return(map)
  })
  createCounties <- reactive({
    box <- st_bbox(reactiveCounties())
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
      addPolygons(
        data = reactiveCounties(),
        color = 'white',
        weight = 1,
        smoothFactor = .3,
        fillOpacity = .75,
        fillColor = ~fillColor,  # Use dynamically updated fillColor
        label = lapply(reactiveCounties()$stateLabels, htmltools::HTML),  # Ensure bold text rendering
        labelOptions = labelOptions(
          style = list(color = 'gray30'),
          textsize = '10px'
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = 'red'
        )
      ) %>%
      fitBounds(
        lng1 = box[["xmin"]], 
        lat1 = box[["ymin"]], 
        lng2 = box[["xmax"]], 
        lat2 = box[["ymax"]]
      ) %>%
      addLegend(
        pal = colorNumeric('Reds', domain = reactiveCounties()[[input$selectedYear]]),
        values = reactiveCounties()[[input$selectedYear]],
        title = '<small>Food Insecurity Scores</small>',
        position = 'bottomleft'
      )
    return(map)
  })
  createDistricts <- reactive({
    box <- st_bbox(reactiveDistricts())
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
      addPolygons(
        data = reactiveDistricts(),
        color = 'white',
        weight = 1,
        smoothFactor = .3,
        fillOpacity = .75,
        fillColor = ~fillColor,  # Use dynamically updated fillColor
        label = lapply(reactiveDistricts()$stateLabels, htmltools::HTML),  # Ensure bold text rendering
        labelOptions = labelOptions(
          style = list(color = 'gray30'),
          textsize = '10px'
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = 'red'
        ),
        layerId = ~FIPS
      ) %>%
      fitBounds(
        lng1 = box[["xmin"]], 
        lat1 = box[["ymin"]], 
        lng2 = box[["xmax"]], 
        lat2 = box[["ymax"]]
      ) %>%
      addLegend(
        pal = colorNumeric('Reds', domain = reactiveDistricts()[[input$selectedYear]]),
        values = reactiveDistricts()[[input$selectedYear]],
        title = '<small>Food Insecurity Scores</small>',
        position = 'bottomleft'
      )
    return(map)
  })
  output$foodInsecurityMap <- renderLeaflet(createStates())
  observeEvent(input$type,{
    
    if(input$type == "State"){
      output$foodInsecurityMap <- renderLeaflet(createStates())
    }
    if(input$type == "County"){
      output$foodInsecurityMap <- renderLeaflet(createCounties())
    }
    if(input$type == "District"){
      
      output$foodInsecurityMap <- renderLeaflet(createDistricts())
    }
    
  }) 
  
  observe({
    level <- input$geographic_level
    
    if (level == "State") {
      updateSelectizeInput(session, "names",
                           label = "Select State(s)",
                           choices = fi_state$state,
                           selected = "Alaska")
    } else if (level == "County") {
      updateSelectizeInput(session, "names",
                           label = "Select County(s)",
                           choices = fi_county$county,
                           selected = "Aleutians East Borough, Alaska")
    } else {
      updateSelectizeInput(session, "names",
                           label = "Select District(s)",
                           choices = fi_districts$district,
                           selected = "Congressional District 1, Alabama")
    }
  })
  
  # Render trendline visualization
  output$trendLine <- renderPlotly({
    if (input$geographic_level == "State") {
      fi_state <- fi_state |> filter(state %in% input$names)
      
      if(length(input$names) > 1) {
        colors <- c("maroon","red")
        
        state_plot <- ggplot(data = fi_state, 
                             mapping = aes(x = year, y = fi, 
                                           group = state, color = state,
                                           text = paste(round(fi * 100,1)," %"))) +
          geom_point() + 
          geom_line() +
          labs(title = "State Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          scale_color_manual(values = colors) +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>State Food Insecurity</b>",
                            "<br>",
                            "<span style='font-size: 15px; color: maroon;'>",
                            input$names[1],
                            "</span>",
                            "<span style='font-size: 15px;'> ,</span>",
                            "<span style='font-size: 15px; color: red;'>",
                            input$names[2],
                            "</span>")
        
      } else {
        state_plot <- ggplot(data = fi_state, 
                             mapping = aes(x = year, y = fi, 
                                           group = state, color = state,
                                           text = paste(round(fi * 100,1),"%"))) +
          geom_point() + 
          geom_line() +
          labs(title = "State Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>State Food Insecurity</b>",
                            "<br><span style='font-size: 15px;'>",
                            input$names,
                            "</span>")
      }
      if(length(input$names) != 0){
        ggplotly(state_plot, tooltip = "text") %>%
          layout(title = list(text = title_text,
                              x = 0.01),
                 margin = list(t = 55, 
                               r = 15,
                               pad = 0))
      }
    } else if (input$geographic_level == "County") {
      fi_county <- fi_county |> filter(county %in% input$names)
      
      if(length(input$names) > 1) {
        colors <- c("maroon","red")
        margins <- list(t = 100,
                        r = 15,
                        pad = 0)
        
        county_plot <- ggplot(data = fi_county, 
                              mapping = aes(x = year, y = fi, 
                                            group = county, color = county,
                                            text = paste(round(fi * 100,1)," %"))) +
          geom_point() + 
          geom_line() +
          labs(title = "County Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          scale_color_manual(values = colors) +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>County Food Insecurity</b>",
                            "<br>",
                            "<span style='font-size: 15px; color: maroon;'>",
                            input$names[1],
                            "</span>",
                            "<span style='font-size: 15px;'> ,</span>",
                            "<br>",
                            "<span style='font-size: 15px; color: red;'>",
                            input$names[2],
                            "</span>")
        
      } else {
        margins <- list(t = 55, 
                        r = 15,
                        pad = 0)
        county_plot <- ggplot(data = fi_county, 
                              mapping = aes(x = year, y = fi, 
                                            group = county, color = county,
                                            text = paste(round(fi * 100,1),"%"))) +
          geom_point() + 
          geom_line() +
          labs(title = "County Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>County Food Insecurity</b>",
                            "<br><span style='font-size: 15px;'>",
                            input$names,
                            "</span>")
      }
      
      ggplotly(county_plot, tooltip = "text") %>%
        layout(title = list(text = title_text,
                            x = 0.01),
               margin = margins)
      
    } else {
      fi_districts <- fi_districts |> filter(district %in% input$names)
      
      if(length(input$names) > 1) {
        colors <- c("maroon","red")
        margins <- list(t = 100,
                        r = 15,
                        pad = 0)
        
        district_plot <- ggplot(data = fi_districts, 
                                mapping = aes(x = year, y = fi, 
                                              group = district, color = district,
                                              text = paste(round(fi * 100,1)," %"))) +
          geom_point() + 
          geom_line() +
          labs(title = "District Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          scale_color_manual(values = colors) +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>District Food Insecurity</b>",
                            "<br>",
                            "<span style='font-size: 15px; color: maroon;'>",
                            input$names[1],
                            "</span>",
                            "<span style='font-size: 15px;'> ,</span>",
                            "<br>",
                            "<span style='font-size: 15px; color: red;'>",
                            input$names[2],
                            "</span>")
        
      } else {
        margins <- list(t = 55, 
                        r = 15,
                        pad = 0)
        
        district_plot <- ggplot(data = fi_districts, 
                                mapping = aes(x = year, y = fi, 
                                              group = district, color = district,
                                              text = paste(round(fi * 100,1),"%"))) +
          geom_point() + 
          geom_line() +
          labs(title = "District Food Insecurity",
               subtitle = input$names,
               x = "Year",
               y = "Food Insecurity Rate (%)") +
          theme_bw() + 
          theme(plot.title = element_text(face = "bold"),
                legend.position = "none")
        
        title_text <- paste("<b>District Food Insecurity</b>",
                            "<br><span style='font-size: 15px;'>",
                            input$names,
                            "</span>")
      }
      if(length(input$names) != 0){
        ggplotly(district_plot, tooltip = "text") %>%
          layout(title = list(text = title_text,
                              x = 0.01),
                 margin = margins)
      }
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
