# This function inputs addresses and returns Child Opportunity Index values 
# based on the census tract

# This function uses tidyverse, readr, censusxy

coi_function <- function(data = x){
  
  library(censusxy)
  library(tidyverse)
  library(readr)
  
  coi_url <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/coi_il_2015.csv"
  
  coi_data <- read_csv(url(coi_url))%>%
                         filter(stateusps == "IL", year == 2015)%>%
                         select(geoid, z_COI_nat, z_ED_nat, z_HE_nat, z_SE_nat)%>%
                         mutate(geoid = as.numeric(geoid))
  
  census_tracts <- cxy_geocode(data, street = "street", city = "city", 
                               state = "state", zip = "zip",
                               return = "geographies", 
                               vintage = "Census2010_Current", 
                               class = "dataframe", output = "full")%>%
    mutate(state = formatC(cxy_state_id, width = 2, flag = "0"), 
           county = formatC(cxy_county_id, width = 3, flag = "0"), 
           tract = formatC(cxy_tract_id, width = 6, flag = "0"),
           geoid = as.numeric(paste0(state, county, tract)))
  
  coi_tract <- left_join(census_tracts, coi_data,
                         by = c("geoid" = "geoid"))
  
  data$coi <- coi_tract$z_COI_nat
  data$lon <- coi_tract$cxy_lon
  data$lat <- coi_tract$cxy_lat
  data$geoid <- coi_tract$geoid
  
  return(data)
  
}