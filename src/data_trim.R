# Trimming COI data set
# Patrick Tu
# 4.21.24

library(dplyr)
library(readr)

years <- c(2012:2023)

coi_data_sub <- read_csv(file = "Data/coi_data_sub.csv")

coi_data_dom <- read_csv(file = "./Data/coi_data_dom.csv")

for (i in 1:length(years)) {
  coi_annual_sub <- coi_data_sub %>%
    filter(year == years[i])
  
  coi_annual_dom <- coi_data_dom %>%
    filter(year == years[i])
  
  write_csv(coi_annual_sub, file = paste0("./Data/", "coi_", years[i], "_sub.csv"))
  
  write_csv(coi_annual_dom, file = paste0("./Data/", "coi_", years[i], "_dom.csv"))
}
