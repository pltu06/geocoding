# Link participant locations to neighborhood indices WITHOUT sending any
# participant data off the machine.
#
# Indices returned (2020 census geography):
#   - SVI 2022 (tract) and SVI 2020 (block group)
#   - ADI 2023 (block group), national and state ranks
#   - COI 3.0 (tract), overall + 3 domains + 14 subdomains, national z-scores
#
# Inputs can be any ONE of:
#   - 12-digit 2020 block group GEOIDs (e.g., from DeGAUSS census_block_group)
#   - 11-digit 2020 tract GEOIDs (block group indices will be NA)
#   - latitude / longitude (block groups are assigned locally with sf)
#
# Why this file exists: the older functions (geo_2020(), geo_2010()) sent full
# addresses to the U.S. Census Bureau geocoder. Under HIPAA Safe Harbor,
# addresses, coordinates, and any geography smaller than a state (county,
# tract, block group) are identifiers, so none of them may be sent to a third
# party. The functions below only ever DOWNLOAD public data:
#   - index CSVs from this GitHub repo (download = TRUE), and
#   - TIGER/Line boundary files via tigris, requested by state only.
# Nothing about the user's data is uploaded. For machines without internet,
# use download = FALSE with data_dir, and bg_shapes with a local boundary file.
#
# This file requires dplyr, readr, stringr, tidyr, and sf (+ tigris if you use
# lat/lon without a local boundary file). Install them yourself, e.g.
#   install.packages(c("dplyr", "readr", "stringr", "tidyr", "sf", "tigris"))

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Helper: turn GEOIDs into zero-padded character strings ----------------------
#
# GEOIDs are codes, not numbers. If they are read as numbers, states 01-09 lose
# their leading zero (e.g., Alabama "010330202001" becomes 10330202001) and no
# longer match the index files. stringr::str_pad() pads with "0".
# (Note: formatC(x, width, flag = "0") pads CHARACTER input with spaces, so we
# don't use it here.)
pad_geoid <- function(x, width) {
  if (is.numeric(x)) {
    # sprintf avoids scientific notation (1.033e+10) for long numbers
    x <- ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  }
  x <- str_trim(as.character(x))
  x[x == ""] <- NA_character_
  str_pad(x, width = width, side = "left", pad = "0")
}

# Helper: Connecticut planning regions -> 2020 counties ------------------------
#
# In 2022 Connecticut replaced its 8 counties (GEOIDs 09001-09015) with 9
# planning regions (09110-09190). The 2020 boundaries, DeGAUSS, the Census
# geocoder, SVI 2020 block groups, and COI use the county codes, but SVI 2022
# and ADI 2023 use the planning-region codes. Tract codes (the 6 digits after
# state + county) did not change and are unique within Connecticut, so the
# tract code alone tells us the 2020 county.
#
#   geoid:  11- or 12-digit GEOIDs (other states are returned unchanged)
#   ct_map: named vector, names = 6-digit tract code, values = 3-digit county
ct_to_county <- function(geoid, ct_map) {
  is_region <- !is.na(geoid) & startsWith(geoid, "09") &
    substr(geoid, 3, 5) >= "110" & substr(geoid, 3, 5) <= "190"
  county <- unname(ct_map[substr(geoid[is_region], 6, 11)])
  # Tracts that don't exist in 2020 (no county found) are left unchanged
  fixed <- paste0("09", county, substr(geoid[is_region], 6, nchar(geoid[is_region])))
  geoid[is_region] <- ifelse(is.na(county), geoid[is_region], fixed)
  geoid
}

# 1. load_indices() ------------------------------------------------------------
#
# Reads and cleans every index table. Involves no user data at all.
# Returns a named list so the tables can be loaded once and reused (pass the
# result to geo_link(indices = ...)).
#
#   coi_year: COI 3.0 year, 2012-2023
#   download: TRUE reads the CSVs from GitHub; FALSE reads from data_dir
#   data_dir: local folder with the repo's Data/ files (used if download = FALSE)
load_indices <- function(coi_year = 2023, download = TRUE, data_dir = "Data") {

  stopifnot("coi_year must be a single year from 2012 to 2023" =
              length(coi_year) == 1 && coi_year %in% 2012:2023)

  if (download) {
    base <- "https://raw.githubusercontent.com/pltu06/geocoding/main/Data"
  } else {
    base <- data_dir
    if (!dir.exists(base)) {
      stop("data_dir '", base, "' does not exist. Point data_dir to a local ",
           "copy of the repo's Data/ folder, or use download = TRUE.")
    }
  }

  # Read only the columns we need (the COI files are ~100 MB), and always read
  # the GEOID key columns as character so leading zeros survive.
  read_index <- function(file, col_select, key) {
    key_type <- list(col_character())
    names(key_type) <- key
    read_csv(file.path(base, file),
             col_select = {{ col_select }},
             col_types = do.call(cols, key_type),
             name_repair = "unique_quiet",
             show_col_types = FALSE, progress = FALSE)
  }

  # SVI 2022, census tract (11-digit FIPS)
  # LOCATION looks like "Census Tract 201; Autauga County; Alabama". The
  # descriptive columns are kept so geo_2020() can return its old output.
  svi_tract <- read_index("svi_2022.csv", c(LOCATION, RPL_THEMES, FIPS), "FIPS") %>%
    rename(svi_tract = RPL_THEMES) %>%
    separate(LOCATION, into = c("census_tract", "county", "state"),
             sep = ";", remove = FALSE) %>%
    # CDC codes missing SVI values as -999
    mutate(svi_tract = if_else(svi_tract == -999, NA_real_, svi_tract),
           FIPS = pad_geoid(FIPS, 11),
           tract = as.factor(gsub("[^0-9.]", "", LOCATION)),
           county_name = sub("\\s+", "", county)) %>%
    select(-county, -state)

  # SVI 2020, census block group (12-digit GEOID)
  svi_bg <- read_index("svi_bg_2020.csv", c(GEOID, THEMES), "GEOID") %>%
    transmute(GEOID = pad_geoid(GEOID, 12), svi_block = THEMES)

  # ADI 2023, census block group (12-digit FIPS)
  # Non-numeric codes mean the rank is suppressed (group quarters, low
  # population, questionable data), so they become NA.
  missing_data <- c("GQ", "PH-GQ", "GQ-PH", "QDI", "PH")

  adi <- read_index("adi_2023.csv", c(FIPS, ADI_NATRANK, ADI_STATERNK), "FIPS") %>%
    mutate(across(starts_with("ADI"), as.character),
           across(starts_with("ADI"), ~ if_else(.x %in% missing_data, NA, .x))) %>%
    transmute(FIPS = pad_geoid(FIPS, 12),
              adi_nat = as.numeric(ADI_NATRANK),
              adi_state = as.numeric(ADI_STATERNK))

  # COI 3.0, census tract (11-digit geoid20): subdomains + domains/overall
  coi_sub <- read_index(paste0("coi_", coi_year, "_sub.csv"),
                        c(geoid20, year, starts_with("z_")), "geoid20")
  coi_dom <- read_index(paste0("coi_", coi_year, "_dom.csv"),
                        c(geoid20, starts_with("z_")), "geoid20")

  coi <- left_join(coi_sub, coi_dom, by = "geoid20") %>%
    rename(coi_year = year) %>%
    mutate(geoid20 = pad_geoid(geoid20, 11))

  # Connecticut: switch SVI 2022 and ADI 2023 from planning-region codes to the
  # 2020 county codes used by everything else (see ct_to_county() above).
  # The tract -> county lookup comes from the COI and SVI block group files.
  ct_tracts <- unique(c(coi$geoid20, substr(svi_bg$GEOID, 1, 11)))
  ct_tracts <- ct_tracts[startsWith(ct_tracts, "09")]
  ct_map <- setNames(substr(ct_tracts, 3, 5), substr(ct_tracts, 6, 11))

  svi_tract$FIPS <- ct_to_county(svi_tract$FIPS, ct_map)
  adi$FIPS <- ct_to_county(adi$FIPS, ct_map)

  list(svi_tract = svi_tract, svi_bg = svi_bg, adi = adi, coi = coi, ct_map = ct_map)
}

# 2. assign_geoids() -----------------------------------------------------------
#
# Finds the 2020 census block group that contains each point, entirely on this
# machine (point-in-polygon with sf). Adds two columns to `data`:
#   geoid_block (12 digits) and geoid_tract (11 digits)
# Row order and row count are unchanged. Rows with missing coordinates, or
# points outside the boundaries, get NA.
#
#   lat, lon:  names of the latitude / longitude columns (WGS84 decimal degrees)
#   states:    state abbreviations or FIPS codes to download boundaries for,
#              e.g. c("IL", "IN"). Only these codes are sent to the Census
#              Bureau's file server. If NULL, states are worked out locally.
#   bg_shapes: optional local block group boundaries instead of downloading:
#              a file path (shapefile, GeoPackage, ...) or an sf object with a
#              GEOID column. Use this on offline machines.
#   year:      boundary year (keep 2020 to match the index data)
assign_geoids <- function(data, lat = "lat", lon = "lon", states = NULL,
                          bg_shapes = NULL, year = 2020) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("assign_geoids() needs the sf package: install.packages(\"sf\")")
  }
  for (col in c(lat, lon)) {
    if (!col %in% names(data)) stop("Column '", col, "' not found in data.")
  }

  lat_v <- suppressWarnings(as.numeric(data[[lat]]))
  lon_v <- suppressWarnings(as.numeric(data[[lon]]))
  has_xy <- !is.na(lat_v) & !is.na(lon_v)

  data$geoid_block <- NA_character_
  data$geoid_tract <- NA_character_

  if (!any(has_xy)) {
    message("No rows have both latitude and longitude; GEOIDs are all NA.")
    return(data)
  }

  # Points to look up. `.row` remembers where each point came from.
  pts <- sf::st_as_sf(
    data.frame(.row = which(has_xy), lon = lon_v[has_xy], lat = lat_v[has_xy]),
    coords = c("lon", "lat"), crs = 4326
  )

  # Get block group boundaries: a local file/object, or a download by state
  if (!is.null(bg_shapes)) {
    bg <- if (inherits(bg_shapes, "sf")) bg_shapes else sf::st_read(bg_shapes, quiet = TRUE)
  } else {
    if (!requireNamespace("tigris", quietly = TRUE)) {
      stop("Downloading boundaries needs the tigris package: ",
           "install.packages(\"tigris\"). Or pass a local file with bg_shapes.")
    }
    if (is.null(states)) {
      # Work out which states the points are in by downloading the (public)
      # state outlines and matching locally. No coordinates are sent anywhere.
      st <- tigris::states(cb = TRUE, year = year, progress_bar = FALSE)
      st_hits <- sf::st_join(sf::st_transform(pts, sf::st_crs(st)), st,
                             join = sf::st_within)
      states <- sort(unique(stats::na.omit(st_hits$STUSPS)))
      message("Downloading ", year, " block groups for: ",
              paste(states, collapse = ", "))
    }
    bg <- do.call(rbind, lapply(states, function(s) {
      tigris::block_groups(state = s, year = year, cb = FALSE, progress_bar = FALSE)
    }))
  }

  # TIGER files name the ID column GEOID, GEOID20, or GEOID10 depending on year
  geoid_col <- intersect(c("GEOID", "GEOID20", "GEOID10"), names(bg))[1]
  if (is.na(geoid_col)) stop("Block group boundaries need a GEOID column.")
  bg <- bg[, geoid_col]

  # Point-in-polygon. st_within finds the block group each point falls inside.
  pts <- sf::st_transform(pts, sf::st_crs(bg))
  hits <- sf::st_drop_geometry(sf::st_join(pts, bg, join = sf::st_within))

  # A point exactly on a shared boundary can match two block groups; keep the
  # first so the output still has one row per input row.
  n_dup <- sum(duplicated(hits$.row))
  if (n_dup > 0) {
    message(n_dup, " point(s) fell on a block group boundary and matched more ",
            "than one block group; the first match was kept.")
    hits <- hits[!duplicated(hits$.row), ]
  }

  data$geoid_block[hits$.row] <- pad_geoid(hits[[geoid_col]], 12)
  data$geoid_tract <- substr(data$geoid_block, 1, 11)

  n_out <- sum(has_xy & is.na(data$geoid_block))
  if (n_out > 0) {
    message(n_out, " point(s) did not fall inside any block group in the ",
            "boundaries used (check `states`, or lat/lon swapped?).")
  }

  data
}

# 3. geo_link() ----------------------------------------------------------------
#
# The main function. Joins SVI, ADI, and COI to each row of `data`.
# Give ONE of: geoid_block, geoid_tract, or lat + lon (column names in `data`).
# If more than one is given, the first in that order is used.
#
# DeGAUSS users: the census_block_group container names its column
# census_block_group_id_2020, so use geoid_block = "census_block_group_id_2020".
#
#   states, bg_shapes:  passed to assign_geoids() when using lat/lon
#   coi_year, download, data_dir: passed to load_indices()
#   indices:  output of load_indices(), to avoid re-reading the CSVs
#   drop_identifiers: TRUE removes address, coordinate, county, and GEOID
#             columns from the output (other columns are left alone, so check
#             for remaining identifiers such as names, dates, or record numbers)
geo_link <- function(data, geoid_block = NULL, geoid_tract = NULL,
                     lat = NULL, lon = NULL, states = NULL, bg_shapes = NULL,
                     coi_year = 2023, download = TRUE, data_dir = "Data",
                     drop_identifiers = FALSE, indices = NULL) {

  stopifnot("data must be a data frame" = is.data.frame(data))
  for (col in c(geoid_block, geoid_tract, lat, lon)) {
    if (!col %in% names(data)) stop("Column '", col, "' not found in data.")
  }

  # Step 1: get a block group and/or tract GEOID for every row ---------------
  if (!is.null(geoid_block)) {
    block <- pad_geoid(data[[geoid_block]], 12)
    tract <- substr(block, 1, 11)
  } else if (!is.null(geoid_tract)) {
    tract <- pad_geoid(data[[geoid_tract]], 11)
    block <- rep(NA_character_, nrow(data))
    message("Only tract GEOIDs were given, so block group indices ",
            "(svi_block, adi_nat, adi_state) will be NA.")
  } else if (!is.null(lat) && !is.null(lon)) {
    located <- assign_geoids(data, lat = lat, lon = lon, states = states,
                             bg_shapes = bg_shapes)
    block <- located$geoid_block
    tract <- located$geoid_tract
  } else {
    stop("Give geoid_block, geoid_tract, or both lat and lon ",
         "(the names of those columns in your data).")
  }

  # Check GEOID lengths BEFORE padding. Padding can only fix a lost leading zero
  # (one missing digit). Anything shorter or longer is not a valid GEOID.
  # (A tract ID given as geoid_block gets padded to 12 digits and then shows up
  # in the "did not match" message below.)
  id_col <- if (!is.null(geoid_block)) geoid_block else geoid_tract
  if (!is.null(id_col)) {
    width <- if (!is.null(geoid_block)) 12 else 11
    raw <- data[[id_col]]
    if (is.numeric(raw)) raw <- ifelse(is.na(raw), NA, sprintf("%.0f", raw))
    raw_len <- nchar(str_trim(as.character(raw)))
    n_bad <- sum(!is.na(raw) & raw_len > 0 & !raw_len %in% c(width - 1, width))
    if (n_bad > 0) {
      warning(n_bad, " value(s) in ", id_col, " are not ", width, "-digit GEOIDs ",
              "and will not match the index data.", call. = FALSE)
    }
  }

  # Step 2: load the index tables (public data only) --------------------------
  if (is.null(indices)) {
    indices <- load_indices(coi_year = coi_year, download = download,
                            data_dir = data_dir)
  }

  # Connecticut GEOIDs that use the 2022 planning regions are switched to the
  # 2020 county codes that the index tables use
  ct_block <- ct_to_county(block, indices$ct_map)
  ct_tract <- ct_to_county(tract, indices$ct_map)
  n_ct <- sum(ct_tract != tract, na.rm = TRUE)
  if (n_ct > 0) {
    message(n_ct, " Connecticut GEOID(s) used 2022 planning-region codes and were ",
            "converted to 2020 county codes.")
  }
  block <- ct_block
  tract <- ct_tract

  # Step 3: join the indices ----------------------------------------------------
  linked <- tibble(geoid_block = block, geoid_tract = tract) %>%
    left_join(select(indices$svi_tract, FIPS, svi_tract),
              by = c("geoid_tract" = "FIPS")) %>%
    left_join(indices$svi_bg, by = c("geoid_block" = "GEOID")) %>%
    left_join(indices$adi, by = c("geoid_block" = "FIPS")) %>%
    left_join(select(indices$coi, geoid20, starts_with("z_") & ends_with("_nat")),
              by = c("geoid_tract" = "geoid20"))

  # Each index file has one row per GEOID, so the row count can't change. This
  # check protects against a future data file with duplicate rows.
  stopifnot(nrow(linked) == nrow(data))

  n_unmatched <- sum(!is.na(tract) & !tract %in% indices$svi_tract$FIPS)
  if (n_unmatched > 0) {
    message(n_unmatched, " row(s) did not match a 2020 census tract. ",
            "Check that the GEOIDs use 2020 boundaries and keep leading zeros.")
  }
  n_missing <- sum(is.na(tract))
  if (n_missing > 0) {
    message(n_missing, " row(s) had no GEOID, so their indices are NA.")
  }

  # Replace any old copies of the output columns, then add the new ones
  overlap <- intersect(names(data), names(linked))
  replaced <- setdiff(overlap, c(geoid_block, geoid_tract))
  if (length(replaced) > 0) {
    message("Replacing existing column(s): ", paste(replaced, collapse = ", "))
  }
  out <- bind_cols(data[setdiff(names(data), overlap)], linked)

  # Step 4 (optional): remove identifier columns -------------------------------
  if (drop_identifiers) {
    out <- drop_identifier_cols(out, extra = c(geoid_block, geoid_tract, lat, lon))
  }

  out
}

# Helper: remove columns that are identifiers under HIPAA Safe Harbor ---------
#
# Matches common address / coordinate / sub-state geography column names,
# including censusxy (cxy_*) and DeGAUSS (matched_*, census_*_id_*) output.
# `state` is kept because states are not identifiers.
drop_identifier_cols <- function(data, extra = NULL) {
  exact <- c("street", "address", "address1", "address2", "city", "zip",
             "zipcode", "zip_code", "lat", "lon", "long", "lng", "latitude",
             "longitude", "county", "county_name", "location", "census_tract",
             "tract", "tract.y", "block", "block_group", "score", "precision",
             "geocode_result")
  prefixes <- "^(cxy_|geoid|matched_|census_.*_id|fips)"

  nm <- tolower(names(data))
  drop <- names(data)[nm %in% exact | nm %in% tolower(extra) | grepl(prefixes, nm)]

  if (length(drop) > 0) {
    message("Dropped identifier column(s): ", paste(drop, collapse = ", "),
            ". Check the remaining columns for other identifiers.")
  }
  data[setdiff(names(data), drop)]
}
