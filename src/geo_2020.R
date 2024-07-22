# This function inputs addresses and outputs Social Vulnerability Index, Child Opportunity Index, 
# and Area Deprivation Index values for each address based on their census block 
# group

# This function requires tidyverse, censusxy, readr, dplyr, tidyr, sf, remotes

geo_2020 <- function(data = NULL, adi_year = 2021, download = TRUE, coi_year = 2021){
  
  stopifnot("You forgot to enter data" = !is.null(data))
  
  packages <- c("remotes", "readr", "dplyr", "tidyr", "sf")
  
  for (i in 1:length(packages)) {
    req <- require(packages[i], character.only = TRUE)
    
    if (req) {
      library(packages[i], character.only = TRUE)
    } else {
      install.packages(packages[i], dependencies = TRUE)
      
      library(packages[i], character.only = TRUE)
    }
  }
  
  test <- require("censusxy", character.only = TRUE)
  
  if (test) {
    library(censusxy)
  } else {
    install_github("chris-prener/censusxy")
    
    library(censusxy)
  }

  if (download) {svi_url_bg <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_bg_2020.csv"
    
  svi_data_bg <- read_csv(url(svi_url_bg))%>%
      select(svi_block = THEMES, GEOID)%>%
      mutate(FIPS = as.numeric(GEOID))
  
  svi_url <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data/svi_2022.csv"
  
  svi_data <- read_csv(url(svi_url))%>%
    select(LOCATION, svi_tract = RPL_THEMES, FIPS)%>%
    separate(
      LOCATION, 
      into = c("census_tract", "county", "state"), 
      sep = ";", 
      remove = FALSE
    ) %>%
    mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
           county_name = sub("\\s+", "", county))%>%
    mutate(FIPS = as.numeric(FIPS))%>%
    select(-county,-state)
  
  adi_url <- paste0("https://raw.githubusercontent.com/pltu06/geocoding/main/Data/adi_il_", adi_year, ".csv")
  
  missing_data <- c("GQ", "PH-GQ", "QDI", "PH")
  
  adi_data <- read_csv(url(adi_url))%>%
    select(-GISJOIN)%>%
    mutate(across(.cols = starts_with("ADI"), 
                  .fns = ~if_else(.x%in%missing_data, NA, .x)))
  
  coi_url <- paste0("https://raw.githubusercontent.com/pltu06/geocoding/main/Data/coi_", coi_year, ".csv")
  
  coi_data <- read_csv(url(coi_url))%>%
    select(geoid20, z_COI_nat, z_ED_nat, z_HE_nat, z_SE_nat)%>%
    mutate(geoid = as.numeric(geoid20))
  
  } else {
    svi_data_bg <- read_csv("/Users/patricktu/Desktop/patrick/Geo/Data/svi_bg_2020.csv")%>%
    select(svi_block = THEMES, GEOID)%>%
    mutate(FIPS = as.numeric(GEOID))
    
    svi_data <- read_csv("/Users/patricktu/Desktop/patrick/Geo/Data/svi_2022.csv")%>%
      select(LOCATION, svi_tract = RPL_THEMES, FIPS)%>%
      separate(
        LOCATION, 
        into = c("census_tract", "county", "state"), 
        sep = ";", 
        remove = FALSE
      ) %>%
      mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
             county_name = sub("\\s+", "", county))%>%
      mutate(FIPS = as.numeric(FIPS))%>%
      select(-county,-state)
    
    missing_data <- c("GQ", "PH-GQ", "QDI", "PH")
    
    adi_data <- read_csv(paste0("/Users/patricktu/Desktop/patrick/Geo/Data/adi_il_", adi_year, ".csv"))%>%
      select(-GISJOIN)%>%
      mutate(across(.cols = starts_with("ADI"), 
                    .fns = ~if_else(.x%in%missing_data, NA, .x)))
    
    coi_data <- read_csv(file = paste0("/Users/patricktu/Desktop/patrick/Geo/Data/coi_", coi_year, ".csv"))%>%
      select(geoid20, z_COI_nat, z_ED_nat, z_HE_nat, z_SE_nat)%>%
      mutate(geoid = as.numeric(geoid20))
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
    left_join(., adi_data, by = c("geoid_block" = "FIPS"))%>%
    left_join(., coi_data, by = c("geoid_tract" = "geoid"))
  
  data$svi_tract <- svi_tract$svi_tract
  data$svi_block <- svi_tract$svi_block
  data$coi <- svi_tract$z_COI_nat
  data$coi_ed <- svi_tract$z_ED_nat
  data$coi_he <- svi_tract$z_HE_nat
  data$coi_se <- svi_tract$z_SE_nat
  data$lon <- svi_tract$cxy_lon
  data$lat <- svi_tract$cxy_lat
  data$geoid_tract <- svi_tract$geoid_tract
  data$geoid_block <- svi_tract$geoid_block
  data$adi_national <- as.numeric(svi_tract$ADI_NATRANK)
  data$adi_state <- as.numeric(svi_tract$ADI_STATERNK)
  data$tract <- svi_tract$tract.y
  
  return(data)
  
}
