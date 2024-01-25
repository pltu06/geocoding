# This function inputs addresses and outputs Social Vulnerability Index values 
# for each address based on their census tract

# This function requires tidyverse, censusxy, readr

svi_xy_function <- function(data = x){
  
  library(censusxy)
  library(tidyverse)
  library(readr)
  
  svi_url <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_il_2020.csv"
  
  svi_data <- read_csv(url(svi_url))%>%
    select(LOCATION, svi = RPL_THEMES, FIPS)%>%
    separate(
      LOCATION, 
      into = c("census_tract", "county", "state"), 
      sep = ",", 
      remove = FALSE
    ) %>%
    mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
           county_name = sub("\\s+", "", county))%>%
    select(-county,-state)
  
  census_tracts <- cxy_geocode(data, street = "street", city = "city", 
                               state = "state", zip = "zip",
                               return = "geographies", 
                               vintage = "Current_Current", 
                               class = "dataframe", output = "full")%>%
    mutate(state = formatC(cxy_state_id, width = 2, flag = "0"), 
           county = formatC(cxy_county_id, width = 3, flag = "0"), 
           tract = formatC(cxy_tract_id, width = 6, flag = "0"),
           geoid = as.numeric(paste0(state, county, tract)))

  svi_tract <- left_join(census_tracts, svi_data,
                         by = c("geoid" = "FIPS"))
  
  data$svi <- svi_tract$svi
  data$lon <- svi_tract$cxy_lon
  data$lat <- svi_tract$cxy_lat
  data$geoid <- svi_tract$geoid
  data$tract <- svi_tract$tract.y
  
  return(data)
  
}