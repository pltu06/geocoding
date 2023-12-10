# This function inputs addresses and outputs Social Vulnerability Index and Area 
# Deprivation Index values for each address based on their census block group

# This function requires tidyverse, censusxy

svi_adi_function <- function(data = x, adi_year = 2021){
  
  library(censusxy)
  library(tidyverse)
  library(readr)
  
  svi_url <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_il_bg_2020.csv"
  
  svi_data <- read_csv(url(svi_url))%>%
    select(svi = THEMES, GEOID)%>%
    mutate(FIPS = as.numeric(GEOID))
  
  adi_url <- paste0("https://raw.githubusercontent.com/pltu06/geocoding/main/Data/adi_il_", adi_year, ".csv")
  
  adi_data <- read_csv(url(adi_url))%>%
    select(-GISJOIN)
  
  census_tracts <- cxy_geocode(x, street = "street", city = "city", 
                               state = "state", zip = "zip",
                               return = "geographies", 
                               vintage = "Current_Current", 
                               class = "dataframe", output = "full")%>%
    mutate(state = formatC(cxy_state_id, width = 2, flag = "0"), 
           county = formatC(cxy_county_id, width = 3, flag = "0"), 
           tract = formatC(cxy_tract_id, width = 6, flag = "0"),
           block_group = substr(cxy_block_id, 1, 1),
           geoid = as.numeric(paste0(state, county, tract, block_group)))
  
  svi_tract <- left_join(census_tracts, svi_data,
                         by = c("geoid" = "FIPS"))%>%
    left_join(., adi_data, by = c("geoid" = "FIPS"))
  
  data$svi <- svi_tract$svi
  data$lon <- svi_tract$cxy_lon
  data$lat <- svi_tract$cxy_lat
  data$geoid <- svi_tract$geoid
  data$adi_national <- svi_tract$ADI_NATRANK
  data$adi_state <- svi_tract$ADI_STATERNK
  
  return(data)
  
}

library(tibble)

x <- tibble(street = c("1501 Washington Ave", "875 N Michigan Ave", 
                       "1901 W Madison St", "8327 N Galena Rd", "951 Chicago Ave"), 
            city = c("Cairo", "Chicago", "Chicago", "Peoria", "Oak Park"),
            state = c("IL", "IL", "IL", "IL", "IL"),
            zip = c(62914, 60611, 60612, 61615, 60302))

svi_adi_function(adi_year = 1920)
