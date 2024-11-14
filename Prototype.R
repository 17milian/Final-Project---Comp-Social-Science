#For Final Code in Development of Prototype
library(readxl)
library(tidyverse)


files <- list.files(pattern="\\.xlsx$",full.names = TRUE)

#Cleaning on State Data
year <- 2008
state.list <- list()
for (file in files) {
  year <- year + 1
  if (year %in% c(2009, 2010, 2019)) {
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
write.csv(state, file = "MMG_State.csv")

