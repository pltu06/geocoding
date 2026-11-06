# Geocoding Tests
# Patrick Tu
# 12.10.23
# Testing Geocoding Functions
#
# Scratch file. The calls below send these public example addresses to the
# U.S. Census Bureau geocoder (allow_external_geocoder = TRUE). Never run it on
# participant data. Tests for geo_link() are in test/.

library(tibble)

# The ten public state capitols used by the tests
source("test/example_data.R")
x <- example_addresses




source("src/geo_link.R")
source("src/geo_2020.R")
source("src/geo_2010.R")


geo_2010(data = x, download = FALSE, allow_external_geocoder = TRUE)

geo_2020(data = x, download = FALSE, allow_external_geocoder = TRUE)

geo_2020(data = x, download = TRUE, allow_external_geocoder = TRUE)



library(censusxy)

data <- stl_homicides

homicide_sf <- cxy_geocode(data, street = "street_address", city = "city", state = "state", 
                             output = "simple", class = "sf")

coi_2023 <- read_csv("./Data/coi_2023.csv")
