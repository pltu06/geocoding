# This function returns Child Opportunity Index (2015) and Social Deprivation Index (2019)
# values based on 2010 census tracts.
#
# Give it ONE of:
#   - geoid_tract: the name of a column with 11-digit 2010 tract GEOIDs
#   - lat + lon:   names of coordinate columns; tracts are found locally with
#                  assign_geoids(year = 2010) from src/geo_link.R
#   - addresses (street, city, state, zip columns) with allow_external_geocoder = TRUE.
#     PRIVACY: this sends every address to the U.S. Census Bureau geocoder, a
#     third party. Do not use it with protected health information.
#
# Needs src/geo_link.R sourced first (for pad_geoid() and assign_geoids()).
#
# KNOWN ISSUE: the COI file this function reads, Data/coi_il_2015.csv, is not in
# the repo. The Data/coi_2015_*.csv files use 2020 tracts, so they can't be
# swapped in. Until a 2010-tract COI file is added, `coi` comes back NA (with a
# warning) and only `sdi` is filled in.

geo_2010 <- function(data = NULL, geoid_tract = NULL, lat = NULL, lon = NULL,
                     download = TRUE, data_dir = "Data", allow_external_geocoder = FALSE){

  stopifnot("You forgot to enter data" = !is.null(data))

  # Without tract IDs or coordinates, the only option left is sending addresses
  # to the Census geocoder, which has to be switched on explicitly
  if (is.null(geoid_tract) && (is.null(lat) || is.null(lon)) && !allow_external_geocoder) {
    stop("Give geoid_tract (2010 tract GEOIDs) or lat and lon. geo_2010() only sends ",
         "addresses to the U.S. Census Bureau geocoder if you set ",
         "allow_external_geocoder = TRUE.", call. = FALSE)
  }

  if (!exists("pad_geoid", mode = "function")) {
    stop("geo_2010() needs src/geo_link.R. Run source(\"src/geo_link.R\") first.",
         call. = FALSE)
  }

  if (download) {
    base <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data"
  } else {
    base <- data_dir
  }

  # COI 2015 on 2010 tracts. Missing from the repo for now (see KNOWN ISSUE).
  coi_data <- tryCatch(
    read_csv(file.path(base, "coi_il_2015.csv"),
             col_types = cols(geoid = col_character()), show_col_types = FALSE) %>%
      filter(stateusps == "IL", year == 2015) %>%
      transmute(geoid = pad_geoid(geoid, 11), z_COI_nat),
    error = function(e) {
      warning("Could not read coi_il_2015.csv (it is not in the repo's Data/ folder), ",
              "so coi will be NA.", call. = FALSE)
      tibble(geoid = character(), z_COI_nat = numeric())
    }
  )

  # SDI 2019 on 2010 tracts. Key kept as character so leading zeros survive.
  sdi_data <- read_csv(file.path(base, "sdi_il_2019.csv"),
                       col_types = cols(CENSUSTRACT_FIPS = col_character()),
                       show_col_types = FALSE) %>%
    transmute(geoid = pad_geoid(CENSUSTRACT_FIPS, 11), sdi)

  # Get a 2010 tract GEOID for each row
  if (!is.null(geoid_tract)) {
    geoid <- pad_geoid(data[[geoid_tract]], 11)
  } else if (!is.null(lat) && !is.null(lon)) {
    geoid <- assign_geoids(data, lat = lat, lon = lon, year = 2010)$geoid_tract
  } else {
    if (!requireNamespace("censusxy", quietly = TRUE)) {
      stop("Geocoding addresses needs censusxy: ",
           "remotes::install_github(\"chris-prener/censusxy\")", call. = FALSE)
    }
    message("Sending ", nrow(data), " address(es) to the U.S. Census Bureau geocoder.")

    # (Earlier versions called cxy_geocode() twice, sending every address twice.)
    census_tracts <- censusxy::cxy_geocode(data, street = "street", city = "city",
                                           state = "state", zip = "zip",
                                           return = "geographies",
                                           vintage = "Census2010_Current",
                                           class = "dataframe", output = "full")
    geoid <- paste0(pad_geoid(census_tracts$cxy_state_id, 2),
                    pad_geoid(census_tracts$cxy_county_id, 3),
                    pad_geoid(census_tracts$cxy_tract_id, 6))
    geoid[is.na(census_tracts$cxy_tract_id)] <- NA_character_
    data$lon <- census_tracts$cxy_lon
    data$lat <- census_tracts$cxy_lat
  }

  coi_tract <- tibble(geoid = geoid) %>%
    left_join(coi_data, by = "geoid") %>%
    left_join(sdi_data, by = "geoid")

  data$coi <- coi_tract$z_COI_nat
  data$geoid <- coi_tract$geoid
  data$sdi <- coi_tract$sdi

  return(data)

}
