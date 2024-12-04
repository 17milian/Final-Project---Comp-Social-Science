library(readxl)
library(tidyverse)


files <- list.files(path="./Raw Data",pattern="\\.xlsx$",full.names = TRUE)

#Cleaning on State Data
year <- 2008
state.list <- list()
for (file in files) {
  year <- year + 1
  if (year %in% c(2009, 2010)) {
    state.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = "State",
                                                   na = c("", "n/a")) |>
                                         mutate(Year = year)
  } else if (year == 2011) {
    state.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = str_c(year, " State "),
                                                   na = c("", "n/a")) |>
                                         mutate(Year = year)
  } else if (year == 2018) {
    state.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = str_c(year, " State"),
                                                   skip = 1,
                                                   na = c("", "n/a")) |>
                                        mutate(Year = year)
  } else if (year == 2019) {
    state.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = "State",
                                                   na = c("", "n/a"))
  } else {
    state.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = str_c(year, " State"),
                                                   na = c("", "n/a")) |>
                                        mutate(Year = year)
  }
}

key <- state.list[["mmg_2011"]] |>
       select(FIPS, State, `County, State`) |>
       rename(`State Name` = State,
              State = `County, State`)

state.list[["mmg_2009"]] <- state.list[["mmg_2009"]] |>
                            rename(State = `State Name`) |>
                            left_join(key) |>
                            select(FIPS, `State Name`, everything())
                          
state.list[["mmg_2010"]] <- state.list[["mmg_2010"]] |>
                            left_join(key, by = "State") |>
                            rename(FIPS = FIPS.y) |>
                            select(-FIPS.x) |>
                            select(FIPS, `State Name`, everything())

state.list[["mmg_2011"]] <- state.list[["mmg_2011"]] |>
                            rename(`State Name` = State,
                                   State = `County, State`)

year <- 2008
for (dat in 1:length(state.list)) {
  year <- year + 1
  state.list[[str_c("mmg_", year)]] <- state.list[[str_c("mmg_", year)]] |>
    select(FIPS, `State Name`, State, Year, 
           everything())
  
  if (year %in% c(2017, 2018)) {
    strings <- c(str_c(" in ", year), str_c(year, " "), " in 2016") 
    positions <- which(str_detect(names(state.list[[str_c("mmg_", year)]]), paste(strings, collapse = "|")))
    old <- state.list[[str_c("mmg_", year)]] |> select(all_of(positions)) |> names()
    new <- str_remove_all(old, paste(strings, collapse = "|"))
    state.list[[str_c("mmg_", year)]] <- state.list[[str_c("mmg_", year)]] |>
      rename_with(~ new[which(all_of(old) == .x)], .cols = old)
  } else {
    strings <- c(str_c(" in ", year), str_c(year, " ")) 
    positions <- which(str_detect(names(state.list[[str_c("mmg_", year)]]), paste(strings, collapse = "|")))
    old <- state.list[[str_c("mmg_", year)]] |> select(all_of(positions)) |> names()
    new <- str_remove_all(old, paste(strings, collapse = "|"))
    state.list[[str_c("mmg_", year)]] <- state.list[[str_c("mmg_", year)]] |>
      rename_with(~ new[which(all_of(old) == .x)], .cols = old) 
  }
}

state.list[["mmg_2009"]] <- state.list[["mmg_2009"]] |>
  rename(`Food Insecurity Rate` = `Food Insecurity Rate (aggregate of counties)`,
         `# of Food Insecure Persons` = `Number Food Insecure Individuals`,
         `Weighted Annual Food Budget Shortfall` = `Weighted Annual Dollars`,
         `% food insecure Children in HH w/HH Incomes Below 185 FPL` = 
           `% of children in FI HH with incomes at or below 185% FPL`,
         `% food insecure Children in HH w/HH Incomes Above 185 FPL` = 
           `% of children in FI HH with incomes above 185% FPL`,
         `Child Food Insecurity Rate` = 
           `Child Food Insecurity Rate (aggregate of counties)`,
         `# of Food Insecure Children` = 
           `Number Food Insecure Children (aggregate of counties)`
  )

state.list[["mmg_2010"]] <- state.list[["mmg_2010"]] |>
  rename(`# of Food Insecure Persons` = 
           `Estimated Number of Food Insecure Persons`,
         `Weighted Annual Food Budget Shortfall` = `Weighted Annual Dollars`,
         `# of Food Insecure Children` = `Number of Food Insecure Children`,
         `% food insecure Children in HH w/HH Incomes Below 185 FPL` = 
           `% of Food Insecure Children in HH w/HH Incomes Below 185 FPL`,
         `% food insecure Children in HH w/HH Incomes Above 185 FPL` =
           `% of Food Insecure Children in HH w/HH Incomes Above 185 FPL`,
         `Weighted Annual Food Budget Shortfall` = `Weighted Annual Dollars`
  )

state.list[["mmg_2011"]] <- state.list[["mmg_2011"]] |>
  rename(`# of Food Insecure Persons` = `Number of Food Insecure Persons`, 
         `# of Food Insecure Children` = `Number of Food Insecure Children`,
         `% food insecure Children in HH w/HH Incomes Below 185 FPL` = 
           `% food insecure children in HH w/ HH incomes below 185 FPL`,
         `% food insecure Children in HH w/HH Incomes Above 185 FPL` =
           `% of food insecure children in HH w/ HH incomes above 185 FPL`
  )

state.list[["mmg_2019"]] <- state.list[["mmg_2019"]] |>
  rename(`Food Insecurity Rate` = `Overall Food Insecurity Rate`, 
         `# of Food Insecure Persons` = `# of Food Insecure Persons Overall`)

year <- 2008
for (dat in 1:(length(state.list) - 1)) {
  year <- year + 1
  state.list[[str_c("mmg_", year)]] <- state.list[[str_c("mmg_", year)]] |>
    select(FIPS, `State Name`, State, Year, 
           `Food Insecurity Rate`, `# of Food Insecure Persons`,
           `Low Threshold in state`, `Low Threshold Type`, 
           `High Threshold in state`, `High Threshold Type`,
           `% FI ≤ Low Threshold`, `% FI Btwn Thresholds`, `% FI > High Threshold`,
           `Child Food Insecurity Rate`, `# of Food Insecure Children`, 
           `% food insecure Children in HH w/HH Incomes Below 185 FPL`,
           `% food insecure Children in HH w/HH Incomes Above 185 FPL`,
           `Cost Per Meal`, `Weighted Annual Food Budget Shortfall`, everything()) |>
    mutate(`Cost Per Meal` = as.character(`Cost Per Meal`),
           `Weighted Annual Food Budget Shortfall` = as.character(`Weighted Annual Food Budget Shortfall`))
}

state.list[["mmg_2019"]] <- state.list[["mmg_2019"]] |>
  select(FIPS, `State Name`, State, Year, 
         `Food Insecurity Rate`, `# of Food Insecure Persons`,
         `% FI ≤ SNAP Threshold`, `SNAP Threshold in State`, `% FI > SNAP Threshold`,
         `Child Food Insecurity Rate`, `# of Food Insecure Children`, 
         `% food insecure Children in HH w/HH Incomes Below 185 FPL`,
         `% food insecure Children in HH w/HH Incomes Above 185 FPL`,
         `Cost Per Meal`, `Weighted Annual Food Budget Shortfall`, everything()) |>
  mutate(`Cost Per Meal` = as.character(`Cost Per Meal`),
         `Weighted Annual Food Budget Shortfall` = as.character(`Weighted Annual Food Budget Shortfall`))

state <- state.list |> reduce(full_join)
write.csv(state, file = "MMG_State.csv", row.names = FALSE)

fi_state <- state |> 
            select(`State Name`, Year, `Food Insecurity Rate`) |> 
            pivot_wider(names_from = Year, values_from = `Food Insecurity Rate`)
write.csv(fi_state, "FI_State.csv", row.names = FALSE)

#Cleaning on County Data
year <- 2008
county.list <- list()
for (file in files) {
  year <- year + 1
  if (year %in% c(2009, 2010)) {
    county.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = "County",
                                                   na = c("", "n/a", "na")) |>
                                         mutate(Year = year)
  } else if (year == 2018) {
    county.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = str_c(year, " County"),
                                                   skip = 1,
                                                   na = c("", "n/a")) |>
                                        mutate(Year = year)
  } else if (year == 2019) {
    county.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                    sheet = "County",
                                                    na = c("", "n/a", "na"))
  } else {
    county.list[[str_c("mmg_",year)]] <- read_excel(file, 
                                                   sheet = str_c(year, " County"),
                                                   na = c("", "n/a", "na")) |>
                                         mutate(Year = year)
  }
}

county.list[["mmg_2010"]]  <- county.list[["mmg_2010"]] |>
                              rename(County = `County, State`)

county.list[["mmg_2009"]] <- county.list[["mmg_2009"]] |> 
                             rename(State = `State Name`, 
                                    County = `County Code`) |> 
                             #68 Rows of N/A
                             filter(!is.na(State)) |>
                             mutate(County = str_to_title(County),
                                    County = case_when(County=="Lake And Peninsula" ~ "Lake and Peninsula",
                                                       County=="Matanuska Susitna" ~ "Matanuska-Susitna",
                                                       County=="Valdez Cordova" ~ "Valdez-Cordova",
                                                       County=="Yukon Koyukuk" ~ "Yukon-Koyukuk",
                                                       TRUE ~ County))

t10 <- county.list[["mmg_2010"]] |> select(FIPS, State, County)                           
t09 <- county.list[["mmg_2009"]] |> select(State, County)

search_1 <- t09 |> 
            group_by(State) |> 
            summarize(counts = n())
search_2 <- t10 |> 
            group_by(State) |> 
            summarize(counts = n())

search <- search_1 |> 
          left_join(search_2, by = "State") |> 
          mutate(diff = abs(counts.x - counts.y)) |> 
          filter(diff != 0)

t10.f <- t10 |> 
         filter(State %in% c("AK","HI")) |> 
         filter(County != "Hawaii County, Hawaii")
         #Filter "Hawaii County" because it has been checked to been included in both, 
         #and to avoid detection and matching to catch "Kalawao County"
t09.f <- t09 |> 
         filter(State %in% c("AK", "HI")) |> 
         filter(County != "Hawaii")

missings <- tibble()
pos <- 0
while (is.na(pos) == FALSE) {
  str_1 <- t10.f$County
  str_2 <- t09.f$County
  
  pos <- which(str_detect(str_1, paste(str_2, collapse="|")) == FALSE)[1]
  
  miss <- t10.f |> slice(pos)
  missings <- rbind(missings, miss)
  
  if (is.na(pos) == TRUE) {
    stop
  } else if (pos == 1) {
    t10.f <- t10.f |> slice(-1:-pos)
  } else {
    t09.f <- t09.f |> slice(-1:-(pos-1))
    t10.f <- t10.f |> slice(-1:-pos) 
  }
}

county.key <- county.list[["mmg_2010"]] |>
              filter(!County %in% missings$County) |>
              select(State, County, FIPS) |>
              group_by(State) |>
              arrange(State) |>
              rename(State.y = State,
                     County.y = County, 
                     FIPS.y = FIPS)

county.list[["mmg_2009"]] <- county.list[["mmg_2009"]] |>
                             cbind(county.key) |>
                             select(-State, -County, -FIPS) |>
                             rename(State = State.y,
                                    `County, State` = County.y,
                                    FIPS = FIPS.y) |>
                             add_row(FIPS = missings$FIPS,
                                     State = missings$State,
                                    `County, State` = missings$County,
                                    Year = 2009) |>
                             select(FIPS, State, `County, State`, everything()) |>
                             group_by(State) |>
                             arrange(State, `County, State`)

county.list[["mmg_2010"]] <- county.list[["mmg_2010"]] |>
                             rename(`County, State` = County)

year <- 2008
for (dat in 1:length(county.list)) {
  year <- year + 1
  county.list[[str_c("mmg_", year)]] <- county.list[[str_c("mmg_", year)]] |>
    select(FIPS, State, `County, State`, Year, 
           everything())
  
  strings <- c(str_c(" in ", year), str_c(year, " ")) 
  positions <- which(str_detect(names(county.list[[str_c("mmg_", year)]]), paste(strings, collapse = "|")))
  old <- county.list[[str_c("mmg_", year)]] |> select(all_of(positions)) |> names()
  new <- str_remove_all(old, paste(strings, collapse = "|"))
  county.list[[str_c("mmg_", year)]] <- county.list[[str_c("mmg_", year)]] |>
                                        rename_with(~ new[which(all_of(old) == .x)], .cols = old)
}

county.list[["mmg_2009"]] <- county.list[["mmg_2009"]] |>
                             rename(`Food Insecurity Rate` = `FI Rate`,
                                     `# of Food Insecure Persons` = `Number Food Insecure Individuals`,
                                     `Weighted Annual Food Budget Shortfall` = `Weighted Annual Dollars`,
                                     `Child food insecurity rate` = `Child FI Rate`,
                                     `% food insecure children in HH w/ HH incomes below 185 FPL` = 
                                      `% of children in FI HH with HH incomes at or below 185% FPL`,
                                     `% food insecure children in HH w/ HH incomes above 185 FPL` = 
                                      `% of children in FI HH with HH incomes above 185% FPL`,
                                     `# of Food Insecure Children` = `Number Food Insecure Children`
                              )


county.list[["mmg_2010"]] <- county.list[["mmg_2010"]] |>
                            rename(`# of Food Insecure Persons` = 
                                     `Number of Food Insecure Persons`,
                                   `# of Food Insecure Children` = `Number of Food Insecure Children`,
                                   `% food insecure children in HH w/ HH incomes above 185 FPL` =
                                     `% of food insecure children in HH w/ HH incomes above 185 FPL`
                            )

county.list[["mmg_2011"]] <- county.list[["mmg_2011"]] |>
                             rename(`# of Food Insecure Persons` = `Number of Food Insecure Persons`,
                                   `# of Food Insecure Children` = `Number of Food Insecure Children`,
                                   `Child food insecurity rate` = `Child Food Insecurity Rate`,
                                   `% food insecure children in HH w/ HH incomes above 185 FPL` =
                                     `% of food insecure children in HH w/ HH incomes above 185 FPL`
                             )

county.list[["mmg_2019"]] <- county.list[["mmg_2019"]] |>
                            rename(`Food Insecurity Rate` = `Overall Food Insecurity Rate`, 
                                   `# of Food Insecure Persons` = `# of Food Insecure Persons Overall`,
                                   `Child food insecurity rate` = `Child Food Insecurity Rate`)

year <- 2008
for (dat in 1:(length(county.list) - 1)) {
  year <- year + 1
  county.list[[str_c("mmg_", year)]] <- county.list[[str_c("mmg_", year)]] |>
    select(FIPS, State, `County, State`, Year, 
           `Food Insecurity Rate`, `# of Food Insecure Persons`,
           `Low Threshold in state`, `Low Threshold Type`, 
           `High Threshold in state`, `High Threshold Type`,
           `% FI ≤ Low Threshold`, `% FI Btwn Thresholds`, `% FI > High Threshold`,
           `Child food insecurity rate`, `# of Food Insecure Children`, 
           `% food insecure children in HH w/ HH incomes below 185 FPL`,
           `% food insecure children in HH w/ HH incomes above 185 FPL`,
           `Cost Per Meal`, `Weighted Annual Food Budget Shortfall`, everything()) |>
    mutate(`% FI Btwn Thresholds` = as.character(`% FI Btwn Thresholds`),
           `Child food insecurity rate` = as.character(`Child food insecurity rate`),
           `Cost Per Meal` = as.character(`Cost Per Meal`),
           `% food insecure children in HH w/ HH incomes below 185 FPL` = 
             as.character(`% food insecure children in HH w/ HH incomes below 185 FPL`),
           `% food insecure children in HH w/ HH incomes above 185 FPL` = 
             as.character(`% food insecure children in HH w/ HH incomes above 185 FPL`),
           `Weighted Annual Food Budget Shortfall` = as.character(`Weighted Annual Food Budget Shortfall`))
}

county.list[["mmg_2019"]] <- county.list[["mmg_2019"]] |>
                             select(FIPS, State, `County, State`, Year, 
                                    `Food Insecurity Rate`, `# of Food Insecure Persons`,
                                    `% FI ≤ SNAP Threshold`, `SNAP Threshold`, `% FI > SNAP Threshold`,
                                    `Child food insecurity rate`, `# of Food Insecure Children`, 
                                    `% food insecure children in HH w/ HH incomes below 185 FPL`,
                                    `% food insecure children in HH w/ HH incomes above 185 FPL`,
                                    `Cost Per Meal`, `Weighted Annual Food Budget Shortfall`, everything()) |>
                             mutate(`Child food insecurity rate` = as.character(`Child food insecurity rate`),
                                    `% food insecure children in HH w/ HH incomes below 185 FPL` = as.character(`% food insecure children in HH w/ HH incomes below 185 FPL`),
                                    `% food insecure children in HH w/ HH incomes above 185 FPL` = as.character(`% food insecure children in HH w/ HH incomes above 185 FPL`),
                                    `Cost Per Meal` = as.character(`Cost Per Meal`),
                                    `Weighted Annual Food Budget Shortfall` = as.character(`Weighted Annual Food Budget Shortfall`))

county <- county.list |> reduce(full_join)
write.csv(county, file = "MMG_County.csv", row.names = FALSE)

fi_county <- county |> 
             select(`County, State`, Year, `Food Insecurity Rate`) |>
             pivot_wider(names_from = Year,
                         values_from = `Food Insecurity Rate`)
write.csv(fi_county, "FI_County.csv", row.names = FALSE)


#Cleaning on District Data
sheet_mapping <- list(
  "MMG2011_2009Data_ToShare.xlsx" = "Congressional_district",
  "MMG2012_2010Data_ToShare.xlsx" = "Congressional District",
  "MMG2013_2011Data_ToShare.xlsx" = "2011 Cong District",
  "MMG2014_2012Data_ToShare.xlsx" = "2012 Cong District",
  "MMG2015_2013Data_ToShare.xlsx" = "2013 Cong District",
  "MMG2016_2014Data_ToShare.xlsx" = "2014 Cong District",
  "MMG2017_2015Data_ToShare.xlsx" = "2015 Cong District",
  "MMG2018_2016Data_ToShare.xlsx" = "2016 Cong District",
  "MMG2019_2017Data_ToShare.xlsx" = "2017 Cong District",
  "MMG2020_2018Data_ToShare.xlsx" = "2018 Cong District",
  "MMG2024_2019-2022_Data_ToShare_v3.xlsx" = "Congressional District"
)

data_list <- list() 
for (file in files) {
  file_name <- basename(file)
  
  if (!file_name %in% names(sheet_mapping)) {
    print(paste("No sheet mapping found for file:", file_name, "Skipping..."))
    next
  }
  
  sheet_name <- sheet_mapping[[file_name]] 
  print(paste("Processing file:", file_name, "Sheet:", sheet_name))
  
  sheet_data <- read_excel(file, sheet = sheet_name)
  
  if (nrow(sheet_data) == 0) {
    print(paste("No data in file:", file_name, "Sheet:", sheet_name))
    next
  }
  
  data_list <- append(data_list, list(sheet_data))
}

if (length(data_list) == 0) {
  stop("No valid data found in the specified sheets!")
}

data_list[[1]] <- data_list[[1]] |>
  rename(FIPS = ID)

#Rewrite MMG 2018 Dataset b/c Load-In Incorrectly
data_list[[10]] <- read_excel("MMG2020_2018Data_ToShare.xlsx", 
                              sheet = "2018 Cong District",
                              skip = 1)

data_list[[11]] <-  data_list[[11]] |>
  rename(`State Name` = State)


year <- 2008
for (dat in 1:length(data_list)) {
  year <- year + 1
  if (year %in% 2009:2018) { #Create Year Column
    data_list[[dat]] <- data_list[[dat]] |>
      mutate(Year = year) |>
      select(FIPS, `State Name`, District, Year, everything())
  } else {
    data_list[[dat]] <- data_list[[dat]] |>
      select(FIPS, `State Name`, District, Year, everything())
  }
  
  #Clean Column Names: Remove Year-Unique Strings
  strings <- c(str_c(" in ", year), str_c(year, " ")) 
  positions <- which(str_detect(names(data_list[[dat]]), paste(strings, collapse = "|")))
  old <- data_list[[dat]] |> select(all_of(positions)) |> names()
  new <- str_remove_all(old, paste(strings, collapse = "|"))
  data_list[[dat]] <- data_list[[dat]] |> 
    rename_with(~ new[which(all_of(old) == .x)], .cols = old)
}

#Clean Column Names: Rename Standard Names
data_list[[1]] <- data_list[[1]] |>
  rename(`Food Insecurity Rate` = `Overall Food Insecurity Rate`,
         `# of Food Insecure Persons` = `Estimated number  food insecure individuals`,
         `# of Food Insecure Children` = `Estimated Number Food Insecure Children`,
         `Low Threshold in state` = `Low Threshold`, `High Threshold in state` = `High Threshold`,
         `% food insecure children in HH w/ HH incomes below 185 FPL` = 
           `% of food insecure children in households with income at or below 185% FPL`,
         `% food insecure children in HH w/ HH incomes above 185 FPL` = 
           `% of food insecure children in households with income above 185% FPL`)

data_list[[2]] <- data_list[[2]] |>
  rename(`# of Food Insecure Persons` = `Estimated Number of Food Insecure Persons`,
         `# of Food Insecure Children` = `Estimated Number of Food Insecure Children`,
         `% food insecure children in HH w/ HH incomes below 185 FPL` = 
           `% of Food Insecure Children in HH w/HH Incomes Below 185 FPL`,
         `% food insecure children in HH w/ HH incomes above 185 FPL` = 
           `% of Food Insecure Children in HH w/HH Incomes Above 185 FPL`)

data_list[[3]] <- data_list[[3]] |>
  rename(`# of Food Insecure Persons` = `Number of Food Insecure Persons`,
         `# of Food Insecure Children` = `Number of Food Insecure Children`,
         `% food insecure children in HH w/ HH incomes above 185 FPL` = 
           `% of food insecure children in HH w/ HH incomes above 185 FPL`)

data_list[[11]] <- data_list[[11]] |>
  rename(`Food Insecurity Rate` = `Overall Food Insecurity Rate`,
         `# of Food Insecure Persons` = `# of Food Insecure Persons Overall`,
         `% food insecure children in HH w/ HH incomes below 185 FPL` = 
           `% food insecure Children in HH w/HH Incomes Below 185 FPL`,
         `% food insecure children in HH w/ HH incomes above 185 FPL` = 
           `% food insecure Children in HH w/HH Incomes Above 185 FPL`
  )

#Sort Columns
for (dat in 2:(length(data_list) - 1)) {
  data_list[[dat]] <- data_list[[dat]] |>
    select(FIPS, `State Name`, District, Year, 
           `Food Insecurity Rate`, `# of Food Insecure Persons`,
           `Low Threshold in state`, `Low Threshold Type`, 
           `High Threshold in state`, `High Threshold Type`,
           `% FI ≤ Low Threshold`, `% FI Btwn Thresholds`, `% FI > High Threshold`,
           `Child Food Insecurity Rate`, `# of Food Insecure Children`, 
           `% food insecure children in HH w/ HH incomes below 185 FPL`,
           `% food insecure children in HH w/ HH incomes above 185 FPL`, everything()) |>
    mutate(`% FI Btwn Thresholds` = as.character(`% FI Btwn Thresholds`))
}

data_list[[1]] <- data_list[[1]] |>
  select(FIPS, `State Name`, District, Year, 
         `Food Insecurity Rate`, `# of Food Insecure Persons`,
         `Low Threshold in state`, `Low Threshold Type`, 
         `High Threshold in state`, `High Threshold Type`,
         `Child Food Insecurity Rate`, `# of Food Insecure Children`, 
         `% food insecure children in HH w/ HH incomes below 185 FPL`,
         `% food insecure children in HH w/ HH incomes above 185 FPL`, everything())

data_list[[11]] <- data_list[[11]] |>
  select(FIPS, `State Name`, District, Year, 
         `Food Insecurity Rate`, `# of Food Insecure Persons`,
         `% FI ≤ SNAP Threshold`, `SNAP Threshold in state`, `% FI > SNAP Threshold`,
         `Child Food Insecurity Rate`, `# of Food Insecure Children`, 
         `% food insecure children in HH w/ HH incomes below 185 FPL`,
         `% food insecure children in HH w/ HH incomes above 185 FPL`, everything())


merged_data <- bind_rows(data_list)
output_file <- "MMG_Districts.csv"
write.csv(merged_data, file = output_file, row.names = FALSE)

fi_merged_data <- merged_data |> 
  select(FIPS, Year, `Food Insecurity Rate`) |> 
  pivot_wider(names_from = Year,
              values_from = `Food Insecurity Rate`)
write.csv(fi_merged_data, "FI_Districts.csv", row.names = FALSE)

