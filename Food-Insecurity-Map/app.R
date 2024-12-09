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
library(shinyWidgets)
library(rsconnect)

source("preprocess.R")



ui <- page_fluid(
  # Enhanced light theme with vibrant colors
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",     # Clean, modern light theme
    bg = "#ffffff",            # White background
    fg = "#333333",            # Dark gray text
    primary = "#4361ee",       # Vibrant blue
    secondary = "#3f37c9",     # Deep purple
    success = "#4cc9f0",       # Bright cyan
    info = "#4895ef",          # Sky blue
    warning = "#f72585",       # Hot pink
    danger = "#ff6b6b",        # Coral red
    base_font = "Segoe UI",     # Modern, friendly font
    "enable-shadows" = TRUE,
    "card-bg" = "#ffffff"      # White card background
  ),
  
  # Header with background image
  card(
    class = "my-3",
    card_header(
      div(
        style = "position: relative; 
           text-align: center; 
           color: white; 
           padding: 37.5px;  # Increased from 18.75px to 37.5px (1.5x)
           box-shadow: 0 4px 8px rgba(0,0,0,0.3); 
           overflow: hidden;",
        div(
          style = "position: absolute; 
             top: 0; 
             left: 0; 
             width: 100%; 
             height: 100%; 
             background-image: url('https://preventioncentre.org.au/wp-content/uploads/2021/10/Food-chequerboard_shutterstock_183253421-900x600.jpg'); 
             background-size: 75%; 
             background-position: center; 
             opacity: 0.75;
             z-index: 0;"
        ),
        div(
          style = "position: relative; z-index: 1;",
          h1("Food Insecurity Across the USA", 
             style = "font-size: 3.15em; font-weight: 700; margin: 0;  # Increased from 2.1em to 3.15em (1.5x)
               text-shadow: 2px 2px 4px rgba(0,0,0,0.5);"),
          p("by Carlos, Allison, Tridib, and Kavya",
            style = "font-size: 1.35em; margin-top: 11.25px; opacity: 1.0;")  # Increased from 0.9em to 1.35em (1.5x)
        )
      )
    )
  ),
  
  # Main content
  navset_card_tab(
    id = "main_tabs",
    header = "Interactive Dashboard",
    height = "100%",
    selected = "Map View",
    nav_panel(
      title = "Map View",
      icon = icon("map"),
      layout_sidebar(
        sidebar = sidebar(
          title = span(icon("gears"), "Control Panel"),
          bg = "#ffffff",
          fg = "#333333",
          border = TRUE,
          width = 350,
          
          # Instructions card
          card(
            class = "mb-3",
            card_header(
              style = "background: linear-gradient(90deg, #4361ee, #3f37c9);",
              class = "text-white",
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                icon("info-circle"), 
                span("Instructions", style = "font-weight: 600;")
              )
            ),
            card_body(
              style = "background-color: #f0f2ff;",  # Light blue-tinted background
              div(
                style = "display: flex; flex-direction: column; gap: 12px;",
                div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  icon("mouse-pointer", style = "color: #4361ee;"), 
                  span("Hover over a state for details", 
                       style = "color: #333333; font-size: 1rem;")
                ),
                div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  icon("calendar", style = "color: #4361ee;"), 
                  span("Select year from the dropdown",
                       style = "color: #333333; font-size: 1rem;")
                )
              )
            )
          ),
          
          # Controls card
          card(
            class = "mb-3",
            card_header(
              style = "background: linear-gradient(90deg, #4cc9f0, #4895ef);",
              class = "text-white",
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                icon("sliders"), 
                span("Map Controls", style = "font-weight: 600;")
              )
            ),
            card_body(
              style = "background-color: #f0fbff;",
              selectInput(
                inputId = "type",
                label = tags$span(
                  icon("map-marked"), 
                  "Select Map Type",
                  style = "color: #333333;"
                ),
                choices = setNames(c("State","County","District"),
                                   c("State","County","District"))
              ),
              selectizeInput(
                inputId = "selectedYear",
                label = tags$span(
                  icon("calendar-alt"), 
                  "Select Year",
                  style = "color: #333333;"
                ),
                choices = setNames(paste0("X", 2009:2022), 2009:2022),
                selected = "X2009",
                options = list(dropdownParent = 'body')
              ),
              conditionalPanel(
                condition = "input.type !== 'State'",
                selectizeInput(
                  inputId = "selectedState",
                  label = tags$span(
                    icon("flag-usa"), 
                    "Select State",
                    style = "color: #333333;"
                  ),
                  choices = setNames(
                    c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                      "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
                      "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                      "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                      "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
                      "56"
                      ),
                    c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                      "Colorado", "Connecticut", "Delaware", "District of Columbia",
                      "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                      "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                      "Maine", "Maryland", "Massachusetts", "Michigan", 
                      "Minnesota", "Mississippi", "Missouri", "Montana", 
                      "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                      "New Mexico", "New York", "North Carolina", "North Dakota",
                      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                      "South Carolina", "South Dakota", "Tennessee", "Texas", 
                      "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
                      "Wisconsin", "Wyoming")
                  ),
                  options = list(dropdownParent = 'body')
                ) 
              )
            )
          )
        ),
        
        # Map card
        card(
          class = "h-100",
          card_header(
            style = "background: linear-gradient(90deg, #4361ee, #4895ef);",
            class = "text-white",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("map-location-dot"), 
              span("Interactive Map", style = "font-weight: 600;")
            )
          ),
          card_body(
            style = "background-color: #ffffff; padding: 0;",
            leafletOutput("foodInsecurityMap", height = "700px")
          )
        )
      )
    ),
    
    nav_panel(
      title = "Analysis",
      icon = icon("chart-simple"),
      layout_sidebar(
        sidebar = sidebar(
          title = span(icon("microscope"), "Analysis Controls"),
          bg = "#ffffff",
          fg = "#333333",
          border = TRUE,
          width = 350,
          
          card(
            class = "mb-3",
            card_header(
              style = "background: linear-gradient(90deg, #f72585, #ff6b6b);",
              class = "text-white",
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                icon("filter"), 
                span("Data Filters", style = "font-weight: 600;")
              )
            ),
            card_body(
              style = "background-color: #fff0f5;",
              radioButtons(
                inputId = "geographic_level",
                label = tags$span(
                  icon("layer-group"), 
                  "Select Level",
                  style = "color: #333333;"
                ),
                choices = c("State", "County", "District"),
                selected = "State"
              ),
              selectizeInput(
                inputId = "names",
                label = tags$span(
                  icon("map-marker-alt"), 
                  "Select Area",
                  style = "color: #333333;"
                ),
                choices = " ",
                multiple = TRUE,
                options = list(maxItems = 2, dropdownParent = 'body')
              )
            )
          )
        ),
        
        # Analysis card
        card(
          class = "h-100",
          card_header(
            style = "background: linear-gradient(90deg, #4cc9f0, #4895ef);",
            class = "text-white",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("chart-line"), 
              span("Trend Analysis", style = "font-weight: 600;")
            )
          ),
          card_body(
            style = "background-color: #ffffff;",
            plotlyOutput(outputId = "trendLine", height = "700px")
          )
        )
      )
    )
  ),
  
  # Footer
  card(
    class = "mt-3",
    card_body(
      style = "background: linear-gradient(90deg, #4361ee, #3f37c9);",
      class = "text-center text-white",
      "Food Insecurity Dashboard © 2024"
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
