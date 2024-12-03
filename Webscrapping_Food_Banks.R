library(rvest)
library(httr)

url <- "https://www.feedingamerica.org/find-your-local-foodbank/all-food-banks"
webpage <- read_html(url)

food_bank_names <- webpage %>%
  html_nodes(".name") %>%
  html_text(trim = TRUE)

print(webpage)

