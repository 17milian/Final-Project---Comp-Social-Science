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

data_list[[2]] <- data_list[[2]] |> mutate('%FI Btwn Thresholds' = as.double('%FI Btwn Thresholds'))


merged_data <- bind_rows(data_list)

output_file <- "merged_districts_data.csv"
write.csv(merged_data, file = output_file, row.names = FALSE)

cat("Data from the specified sheets has been merged and saved to", output_file, "\n")

