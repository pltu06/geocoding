# This function inputs addresses and outputs Social Vulnerability Index and Area 
# Deprivation Index values for each address based on their census block group

# This function requires tidyverse, censusxy, readr

svi_adi_function <- function(data = x, adi_year = 2021, download = TRUE){
  
  library(censusxy)
  library(tidyverse)
  library(readr)

  if (download) {svi_url_bg <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_il_bg_2020.csv"
    
  svi_data_bg <- read_csv(url(svi_url_bg))%>%
      select(svi_block = THEMES, GEOID)%>%
      mutate(FIPS = as.numeric(GEOID))
  
  svi_url <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_il_2020.csv"
  
  svi_data <- read_csv(url(svi_url))%>%
    select(LOCATION, svi_tract = RPL_THEMES, FIPS)%>%
    separate(
      LOCATION, 
      into = c("census_tract", "county", "state"), 
      sep = ",", 
      remove = FALSE
    ) %>%
    mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
           county_name = sub("\\s+", "", county))%>%
    select(-county,-state)
  
  adi_url <- paste0("https://raw.githubusercontent.com/pltu06/geocoding/main/Data/adi_il_", adi_year, ".csv")
  
  missing_data <- c("GQ", "PH-GQ", "QDI", "PH")
  
  adi_data <- read_csv(url(adi_url))%>%
    select(-GISJOIN)%>%
    mutate(across(.cols = starts_with("ADI"), 
                  .fns = ~if_else(.x%in%missing_data, NA, .x)))
  } else {
    svi_data_bg <- read_csv("Data/svi_il_bg_2020.csv")%>%
    select(svi_block = THEMES, GEOID)%>%
    mutate(FIPS = as.numeric(GEOID))
    
    svi_data <- read_csv("Data/svi_il_2020.csv")%>%
      select(LOCATION, svi_tract = RPL_THEMES, FIPS)%>%
      separate(
        LOCATION, 
        into = c("census_tract", "county", "state"), 
        sep = ",", 
        remove = FALSE
      ) %>%
      mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
             county_name = sub("\\s+", "", county))%>%
      select(-county,-state)
    
    missing_data <- c("GQ", "PH-GQ", "QDI", "PH")
    
    adi_data <- read_csv(paste0("Data/adi_il_", adi_year, ".csv"))%>%
      select(-GISJOIN)%>%
      mutate(across(.cols = starts_with("ADI"), 
                    .fns = ~if_else(.x%in%missing_data, NA, .x)))
  }
  
  
  
  census_tracts <- cxy_geocode(data, street = "street", city = "city", 
                               state = "state", zip = "zip",
                               return = "geographies", 
                               vintage = "Current_Current", 
                               class = "dataframe", output = "full")%>%
    mutate(state = formatC(cxy_state_id, width = 2, flag = "0"), 
           county = formatC(cxy_county_id, width = 3, flag = "0"), 
           tract = formatC(cxy_tract_id, width = 6, flag = "0"),
           block_group = substr(cxy_block_id, 1, 1),
           geoid_block = as.numeric(paste0(state, county, tract, block_group)),
           geoid_tract = as.numeric(paste0(state, county, tract)))
  
  svi_tract <- left_join(census_tracts, svi_data,
                         by = c("geoid_tract" = "FIPS"))%>%
    left_join(., svi_data_bg, by = c("geoid_block" = "FIPS"))%>%
    left_join(., adi_data, by = c("geoid_block" = "FIPS"))
  
  data$svi_tract <- svi_tract$svi_tract
  data$svi_block <- svi_tract$svi_block
  data$lon <- svi_tract$cxy_lon
  data$lat <- svi_tract$cxy_lat
  data$geoid_tract <- svi_tract$geoid_tract
  data$geoid_block <- svi_tract$geoid_block
  data$adi_national <- as.numeric(svi_tract$ADI_NATRANK)
  data$adi_state <- as.numeric(svi_tract$ADI_STATERNK)
  data$tract <- svi_tract$tract.y
  
  return(data)
  
}
