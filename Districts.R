library(readxl)
library(dplyr)

excel_files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

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

for (file in excel_files) {
  file_name <- basename(file)  # Get the file name (without path)
  
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

#cat("Data from the specified sheets has been merged and saved to", output_file, "\n")

