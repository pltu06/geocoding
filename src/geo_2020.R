# This function inputs addresses and outputs Social Vulnerability Index, Child Opportunity Index,
# and Area Deprivation Index values for each address based on their census block
# group
#
# PRIVACY: this function sends every address to the U.S. Census Bureau geocoder
# (a third party with no business associate agreement). Do not use it with
# protected health information. It only runs if you set
# allow_external_geocoder = TRUE.
#
# For sensitive data, geocode locally (e.g., DeGAUSS, https://degauss.org) and
# use geo_link() in src/geo_link.R, which never sends data off your machine.
#
# This function requires src/geo_link.R to be sourced first, plus censusxy:
#   remotes::install_github("chris-prener/censusxy")

geo_2020 <- function(data = NULL, download = TRUE, coi_year = 2023, data_dir = "Data",
                     allow_external_geocoder = FALSE){

  stopifnot("You forgot to enter data" = !is.null(data))

  if (!allow_external_geocoder) {
    stop("geo_2020() sends addresses to the U.S. Census Bureau geocoder, a third-party ",
         "service. To keep data on your machine, geocode locally (e.g., DeGAUSS, ",
         "https://degauss.org) and use geo_link() instead. If your data are not ",
         "sensitive and you accept sending addresses to the Census Bureau, rerun with ",
         "allow_external_geocoder = TRUE.", call. = FALSE)
  }

  if (!exists("geo_link", mode = "function")) {
    stop("geo_2020() needs geo_link(). Run source(\"src/geo_link.R\") first, or ",
         "source(\"https://raw.githubusercontent.com/pltu06/geocoding/main/src/geo_link.R\").",
         call. = FALSE)
  }

  if (!requireNamespace("censusxy", quietly = TRUE)) {
    stop("geo_2020() needs censusxy: remotes::install_github(\"chris-prener/censusxy\")",
         call. = FALSE)
  }

  message("Sending ", nrow(data), " address(es) to the U.S. Census Bureau geocoder.")

  indices <- load_indices(coi_year = coi_year, download = download, data_dir = data_dir)

  # vintage = "Current_Current" is kept so results match earlier versions of this
  # function. Note: Connecticut switched from counties to planning regions in
  # 2022, so current Connecticut GEOIDs will not match the 2020 index data.
  census_tracts <- censusxy::cxy_geocode(data, street = "street", city = "city",
                                         state = "state", zip = "zip",
                                         return = "geographies",
                                         vintage = "Current_Current",
                                         class = "dataframe", output = "full")%>%
    # Build the 12-digit block group GEOID: state (2) + county (3) + tract (6) +
    # block group (the first digit of the 4-digit block number)
    mutate(geoid_block = paste0(pad_geoid(cxy_state_id, 2),
                                pad_geoid(cxy_county_id, 3),
                                pad_geoid(cxy_tract_id, 6),
                                substr(pad_geoid(cxy_block_id, 4), 1, 1)),
           # Addresses that didn't geocode should stay NA, not "NANANA..."
           geoid_block = if_else(is.na(cxy_block_id), NA_character_, geoid_block))

  # geo_link() adds geoid_tract and all of the index columns
  svi_tract <- geo_link(census_tracts, geoid_block = "geoid_block", indices = indices) %>%
    # Add the descriptive tract columns the old version of this function returned
    left_join(select(indices$svi_tract, FIPS, LOCATION, census_tract,
                     tract.y = tract, county_name),
              by = c("geoid_tract" = "FIPS")) %>%
    select(street:zip, cxy_lon, cxy_lat, tract.y, geoid_block, geoid_tract,
           LOCATION, census_tract, svi_tract, county_name, svi_block, adi_nat, adi_state,
           starts_with("z_") & ends_with("_nat"))

  return(svi_tract)

}
