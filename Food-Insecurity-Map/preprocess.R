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