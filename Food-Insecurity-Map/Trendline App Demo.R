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

# Read and preprocess the data
state_data <- read.csv("states_FI_graph.csv")
states <- read_sf("cb_2019_us_state_5m.shp")
district_data <- read.csv("FI_Districts.csv")
districts <- read_sf("districts.shp")
if (!all(is.element(state_data$state, states$NAME))) {
  stop("Some states in state_data are not present in the shapefile.")
}

fi_state <- read.csv("Analysis/Analysis_State.csv")
fi_county <- read.csv("Analysis/Analysis_County.csv")
fi_districts <- read.csv("Analysis/Analysis_District.csv")

#id = str_remove(id, "^0+")
#merged_districts <- merge(districts, district_data,)


states <- merge(states, state_data, by.x = 'NAME', by.y = 'state', all.x = FALSE)

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
          
          ggplotly(state_plot, tooltip = "text") %>%
            layout(title = list(text = title_text,
                                x = 0.01),
                   margin = list(t = 55, 
                                 r = 15,
                                 pad = 0))
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
          
          ggplotly(district_plot, tooltip = "text") %>%
            layout(title = list(text = title_text,
                                x = 0.01),
                   margin = margins)
          
        }
      })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
