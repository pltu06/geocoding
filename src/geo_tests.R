# Geocoding Tests
# Patrick Tu
# 12.10.23
# Testing Geocoding Functions

library(tibble)

x <- tibble(street = c("1501 Washington Ave", "875 N Michigan Ave", 
                       "1901 W Madison St", "8327 N Galena Rd", "951 Chicago Ave"), 
            city = c("Cairo", "Chicago", "Chicago", "Peoria", "Oak Park"),
            state = c("IL", "IL", "IL", "IL", "IL"),
            zip = c(62914, 60611, 60612, 61615, 60302))




source("src/geo_2020.R")
source("src/geo_2010.R")


geo_2010(data = x, download = FALSE)

geo_2020(data = x, adi_year = 2020, download = FALSE)

geo_2020(data = x, download = FALSE)

geo_merge_function()

geo_2020(data = x, adi_year = 2021, download = FALSE)
