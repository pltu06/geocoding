# Trimming COI data set
# Patrick Tu
# 4.21.24

library(dplyr)
library(readr)

years <- c(2012:2021)

coi_data <- read_csv(file = "Data/coi_2024.csv")

for (i in 1:length(years)) {
  coi_annual <- coi_data %>%
    filter(year == years[i])
  
  write_csv(coi_annual, file = paste0("./Data/", "coi_", years[i], ".csv"))
}
