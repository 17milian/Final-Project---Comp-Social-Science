library(tidyverse)
library(plotly)

#Placeholder Visualizations

#Trend Line(s)
  #Notes:
  #Implement Function(s)
  #Allow input for different geographic levels (state, county, district) like mapping
  #Either create input or connect input from mapping
    #Inputs have to be character types, or surrounded by ""
    #Suggest Dropdown Menu?


## State
state_plot <- function(nams) {
  fi_state <- read.csv("FI_State.csv") 
  
  for (col in colnames(fi_state)) {
    if(col == "State.Name") {
      fi_state <- fi_state |>
                  rename(state = col)
    } else {
      new <- str_remove(col, "X")
      fi_state <- fi_state |>
                  rename_with(~ new[which(col == .x)], .cols = col)
    }
  }
  
  fi_state <- fi_state |> 
              pivot_longer(
                cols = !state,
                names_to = "year",
                values_to = "fi") |>
              filter(state %in% nams)
  
  if(length(nams) > 1) {
    colors <- c("maroon","red")
    
    state_plot <- ggplot(data = fi_state, 
                         mapping = aes(x = year, y = fi, 
                                       group = state, color = state,
                                       text = paste(round(fi * 100,1)," %"))) +
                  geom_point() + 
                  geom_line() +
                  labs(title = "State Food Insecurity",
                       subtitle = nams,
                       x = "Year",
                       y = "Food Insecurity Rate (%)") +
                  scale_color_manual(values = colors) +
                  theme_bw() + 
                  theme(plot.title = element_text(face = "bold"),
                        legend.position = "none")
    
    title_text <- paste("<b>State Food Insecurity</b>",
                        "<br>",
                        "<span style='font-size: 15px; color: maroon;'>",
                        nams[1],
                        "</span>",
                        "<span style='font-size: 15px;'> ,</span>",
                        "<span style='font-size: 15px; color: red;'>",
                        nams[2],
                        "</span>")
 
  } else {
    state_plot <- ggplot(data = fi_state, 
                         mapping = aes(x = year, y = fi, 
                                       group = state, color = state,
                                       text = paste(round(fi * 100,1),"%"))) +
                        geom_point() + 
                        geom_line() +
                        labs(title = "State Food Insecurity",
                             subtitle = nams,
                             x = "Year",
                             y = "Food Insecurity Rate (%)") +
                        theme_bw() + 
                        theme(plot.title = element_text(face = "bold"),
                              legend.position = "none")
    
    title_text <- paste("<b>State Food Insecurity</b>",
                        "<br><span style='font-size: 15px;'>",
                        nams,
                        "</span>")
  }
  
  ggplotly(state_plot, tooltip = "text") %>%
    layout(title = list(text = title_text,
                        x = 0.01),
           margin = list(t = 55, 
                         r = 15,
                         pad = 0))
  
}

#state_plot("Alaska")
#state_plot(c("Alaska","Alabama"))

## County
county_plot <- function(nams) {
  fi_county <- read.csv("FI_County.csv") |>
               select(-State)
  
  for (col in colnames(fi_county)) {
    if(col == "County..State") {
      fi_county <- fi_county |>  
                   rename(county = "County..State")
    } else {
      new <- str_remove(col, "X")
      fi_county <- fi_county |>
        rename_with(~ new[which(col == .x)], .cols = col)
    }
  }
  
  fi_county <- fi_county |> 
    pivot_longer(
      cols = !county,
      names_to = "year",
      values_to = "fi") |>
    filter(county %in% nams)
  
  if(length(nams) > 1) {
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
           subtitle = nams,
           x = "Year",
           y = "Food Insecurity Rate (%)") +
      scale_color_manual(values = colors) +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
    
    title_text <- paste("<b>County Food Insecurity</b>",
                        "<br>",
                        "<span style='font-size: 15px; color: maroon;'>",
                        nams[1],
                        "</span>",
                        "<span style='font-size: 15px;'> ,</span>",
                        "<br>",
                        "<span style='font-size: 15px; color: red;'>",
                        nams[2],
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
           subtitle = nams,
           x = "Year",
           y = "Food Insecurity Rate (%)") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
    
    title_text <- paste("<b>County Food Insecurity</b>",
                        "<br><span style='font-size: 15px;'>",
                        nams,
                        "</span>")
  }
  
  ggplotly(county_plot, tooltip = "text") %>%
    layout(title = list(text = title_text,
                        x = 0.01),
           margin = margins)
  
}

#county_plot("Aleutians East Borough, Alaska")
#county_plot(c("Aleutians East Borough, Alaska", "Aleutians West Census Area, Alaska"))

## District
district_plot <- function(nams) {
  fi_districts <- read.csv("FI_Districts.csv") 
  districts <- read.csv("MMG_Districts.csv") |>
               select(FIPS, District) |> 
               mutate(District = str_remove(District, " \\(.*\\)")) |>
               filter(!District == "State Rate - Aggregated") |> 
               distinct(FIPS, .keep_all = TRUE)
               
  
  fi_districts <- fi_districts |>
                  left_join(districts, by = join_by(FIPS == FIPS)) |>
                  select(FIPS, District, everything())
  
  for (col in colnames(fi_districts)) {
    if(col == "District") {
      fi_districts <- fi_districts |>
        rename(district = col)
    } else {
      new <- str_remove(col, "X")
      fi_districts <- fi_districts |>
                      rename_with(~ new[which(col == .x)], .cols = col)
    }
  }
  
  fi_districts <- fi_districts |> 
                  pivot_longer(
                    cols = !c(district, FIPS),
                    names_to = "year",
                    values_to = "fi") |>
                  filter(district %in% nams)
  
  if(length(nams) > 1) {
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
           subtitle = nams,
           x = "Year",
           y = "Food Insecurity Rate (%)") +
      scale_color_manual(values = colors) +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
    
    title_text <- paste("<b>District Food Insecurity</b>",
                        "<br>",
                        "<span style='font-size: 15px; color: maroon;'>",
                        nams[1],
                        "</span>",
                        "<span style='font-size: 15px;'> ,</span>",
                        "<br>",
                        "<span style='font-size: 15px; color: red;'>",
                        nams[2],
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
           subtitle = nams,
           x = "Year",
           y = "Food Insecurity Rate (%)") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
    
    title_text <- paste("<b>District Food Insecurity</b>",
                        "<br><span style='font-size: 15px;'>",
                        nams,
                        "</span>")
  }
  
  ggplotly(district_plot, tooltip = "text") %>%
    layout(title = list(text = title_text,
                        x = 0.01),
           margin = margins)
  
}

#district_plot("Congressional District 1, Alabama")
#district_plot(c("Congressional District 1, Alabama",
                "Congressional District 2, Alabama"))

#Linear Regression
