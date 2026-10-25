# This function inputs addresses and outputs Social Vulnerability Index, Child Opportunity Index, 
# and Area Deprivation Index values for each address based on their census block 
# group

# This function requires tidyverse, censusxy, readr, dplyr, tidyr, sf, remotes

geo_2020 <- function(data = NULL, download = TRUE, coi_year = 2023, data_dir = "Data"){
  
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

  # Read data from GitHub, or from a local copy of the repo's Data/ folder
  if (download) {
    base <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data"
  } else {
    base <- data_dir
  }

  svi_data_bg <- read_csv(file.path(base, "svi_bg_2020.csv"))%>%
    select(svi_block = THEMES, GEOID)
  
  svi_data <- read_csv(file.path(base, "svi_2022.csv"))%>%
    select(LOCATION, svi_tract = RPL_THEMES, FIPS)%>%
    separate(
      LOCATION, 
      into = c("census_tract", "county", "state"), 
      sep = ";", 
      remove = FALSE
    ) %>%
    mutate(tract = as.factor(gsub("[^0-9.]", "", LOCATION)), 
           county_name = sub("\\s+", "", county))%>%
    select(-county,-state)
  
  missing_data <- c("GQ", "PH-GQ", "QDI", "PH")
  
  adi_data <- read_csv(file.path(base, "adi_2023.csv"))%>%
    select(-GISJOIN)%>%
    mutate(across(.cols = starts_with("ADI"), 
                  .fns = ~if_else(.x%in%missing_data, NA, .x))) %>%
    mutate(FIPS = as.character(FIPS))
  
  coi_data_sub <- read_csv(file.path(base, paste0("coi_", coi_year, "_sub.csv")))%>%
    select(geoid20, coi_year = year, starts_with("z_"))
  
  coi_data_dom <- read_csv(file.path(base, paste0("coi_", coi_year, "_dom.csv")))%>%
    select(geoid20, coi_year = year, starts_with("z_"))
  
  coi_data <- left_join(coi_data_sub, coi_data_dom, by = c("geoid20"))
  
  census_tracts <- cxy_geocode(data, street = "street", city = "city", 
                               state = "state", zip = "zip",
                               return = "geographies", 
                               vintage = "Current_Current",
                               class = "dataframe", output = "full")%>%
    mutate(state_fips = formatC(cxy_state_id, width = 2, flag = "0"), 
           county_fips = formatC(cxy_county_id, width = 3, flag = "0"), 
           tract_fips = formatC(cxy_tract_id, width = 6, flag = "0"),
           block_group = substr(cxy_block_id, 1, 1),
           geoid_block = paste0(state_fips, county_fips, tract_fips, block_group),
           geoid_tract = paste0(state_fips, county_fips, tract_fips))
  
  svi_tract <- left_join(census_tracts, svi_data,
                         by = c("geoid_tract" = "FIPS")) %>%
    left_join(., svi_data_bg, by = c("geoid_block" = "GEOID")) %>%
    left_join(., adi_data, by = c("geoid_block" = "FIPS")) %>%
    left_join(., coi_data, by = c("geoid_tract" = "geoid20")) %>%
    mutate(adi_nat = as.numeric(ADI_NATRANK), adi_state = as.numeric(ADI_STATERNK)) %>%
    select(street:zip, cxy_lon, cxy_lat, tract.y = tract, geoid_block:svi_block, adi_nat, adi_state, starts_with("z_") & ends_with("_nat"))
  
  return(svi_tract)
  
}
