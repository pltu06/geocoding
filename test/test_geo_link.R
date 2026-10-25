# Tests for geo_link(), assign_geoids(), and load_indices()
# Run from the repo root: Rscript test/test_geo_link.R
#
# Index data are read from the local Data/ folder (download = FALSE), so the
# only network use is the lat/lon tests downloading block group boundaries
# with tigris for the ten example states (only the state codes are sent).
#
# All locations are the public state capitols in test/example_data.R.
# A few extra census IDs (no addresses) test leading zeros and Connecticut.

library(testthat)

source("src/geo_link.R")
source("src/geo_2020.R")
source("test/example_data.R")

indices <- load_indices(coi_year = 2023, download = FALSE, data_dir = "Data")

places <- example_coords
expected <- example_expected

test_that("index tables have character, zero-padded keys", {
  expect_type(indices$svi_tract$FIPS, "character")
  expect_type(indices$svi_bg$GEOID, "character")
  expect_type(indices$adi$FIPS, "character")
  expect_type(indices$coi$geoid20, "character")
  expect_true(all(nchar(indices$svi_tract$FIPS) == 11))
  expect_true(all(nchar(indices$svi_bg$GEOID) == 12))
  expect_true(all(nchar(indices$adi$FIPS) == 12))
  expect_true(all(nchar(indices$coi$geoid20) == 11))
  # Alabama (state 01) keeps its leading zero
  expect_true("010330202001" %in% indices$svi_bg$GEOID)
})

test_that("pad_geoid() pads numbers and strings with zeros, keeps NA", {
  expect_equal(pad_geoid(10330202001, 12), "010330202001")
  expect_equal(pad_geoid("10330202001", 12), "010330202001")
  expect_equal(pad_geoid(c("6081610700", NA, ""), 11), c("06081610700", NA, NA))
})

test_that("geo_link() works from block group GEOIDs (DeGAUSS column name)", {
  d <- tibble(id = places$id, census_block_group_id_2020 = expected$geoid_block)
  out <- geo_link(d, geoid_block = "census_block_group_id_2020", indices = indices)

  expect_equal(nrow(out), nrow(places))
  expect_equal(out$id, places$id)
  expect_equal(out$geoid_tract, substr(expected$geoid_block, 1, 11))
  expect_equal(out$svi_tract, expected$svi_tract)
  expect_equal(out$adi_nat, expected$adi_nat)
  expect_equal(out$adi_state, expected$adi_state)
  expect_false(anyNA(out$svi_block))
  expect_false(anyNA(out$z_COI_nat))
  expect_equal(sum(grepl("^z_.*_nat$", names(out))), 18)
})

test_that("geo_link() works from tract GEOIDs; block group indices are NA", {
  d <- tibble(id = places$id, tract = substr(expected$geoid_block, 1, 11))
  expect_message(out <- geo_link(d, geoid_tract = "tract", indices = indices),
                 "Only tract GEOIDs")
  expect_equal(out$svi_tract, expected$svi_tract)
  expect_false(anyNA(out$z_COI_nat))
  expect_true(all(is.na(out$svi_block)))
  expect_true(all(is.na(out$adi_nat)))
})

test_that("geo_link() pads GEOIDs from a state below 10 (Alabama)", {
  # Same block group given as a number (leading zero lost) and as text
  d <- tibble(bg = c(10330202001, NA))
  out <- geo_link(d, geoid_block = "bg", indices = indices)
  expect_equal(out$geoid_block, c("010330202001", NA))
  expect_equal(out$geoid_tract, c("01033020200", NA))
  expect_false(is.na(out$svi_block[1]))
  expect_false(is.na(out$svi_tract[1]))
  expect_false(is.na(out$z_COI_nat[1]))

  out_chr <- geo_link(tibble(bg = "010330202001"), geoid_block = "bg", indices = indices)
  expect_equal(out_chr$svi_block, out$svi_block[1])
})

test_that("SVI's -999 missing code becomes NA", {
  expect_false(any(indices$svi_tract$svi_tract == -999, na.rm = TRUE))
  expect_true(anyNA(indices$svi_tract$svi_tract))
})

test_that("Connecticut planning-region and 2020 county GEOIDs give the same indices", {
  # Same block group: 2020 Hartford County (09003) and 2022 Capitol Planning Region (09110)
  d <- tibble(county_code = "090035021002", region_code = "091105021002")
  by_county <- geo_link(d, geoid_block = "county_code", indices = indices)
  expect_message(by_region <- geo_link(d, geoid_block = "region_code", indices = indices),
                 "planning-region")
  expect_equal(by_region$geoid_block, "090035021002")
  expect_false(anyNA(by_county[c("svi_tract", "svi_block", "adi_nat", "z_COI_nat")]))
  expect_equal(by_region[-(1:2)], by_county[-(1:2)])
})

test_that("geo_link() reports GEOIDs that don't match and wrong lengths", {
  expect_message(geo_link(tibble(bg = "999999999999"), geoid_block = "bg", indices = indices),
                 "did not match a 2020 census tract")
  # A tract ID given as a block group ID can't match
  expect_message(geo_link(tibble(bg = "17167001300"), geoid_block = "bg", indices = indices),
                 "did not match a 2020 census tract")
  expect_warning(geo_link(tibble(bg = "1716700130"), geoid_block = "bg", indices = indices),
                 "not 12-digit GEOIDs")
})

test_that("geo_link() errors when no input is given", {
  expect_error(geo_link(places, indices = indices), "Give geoid_block")
  expect_error(geo_link(places, geoid_block = "nope", indices = indices), "not found")
})

# lat/lon tests need block group boundaries for the ten states (downloaded
# once by state and cached)
options(tigris_use_cache = TRUE)
example_bg <- do.call(rbind, lapply(places$state, function(s) {
  tigris::block_groups(state = s, year = 2020, progress_bar = FALSE)
}))

test_that("assign_geoids() keeps row order and count; NA coords give NA", {
  # Shuffle the rows and add a row without coordinates
  shuffled <- c(7, 3, 10, 1, 5, 9, 2, 6, 4, 8)
  d <- bind_rows(places[shuffled, ], tibble(id = 11L, lat = NA, lon = NA))
  out <- assign_geoids(d, bg_shapes = example_bg)

  expect_equal(nrow(out), 11)
  expect_equal(out$id, c(shuffled, 11))
  expect_equal(out$geoid_block[1:10], expected$geoid_block[shuffled])
  expect_true(is.na(out$geoid_block[11]))
  expect_true(is.na(out$geoid_tract[11]))
})

test_that("geo_link() from lat/lon matches the old geo_2020() results", {
  # Local boundaries
  out <- geo_link(places, lat = "lat", lon = "lon", bg_shapes = example_bg, indices = indices)
  expect_equal(out$geoid_block, expected$geoid_block)
  expect_equal(out$svi_tract, expected$svi_tract)
  expect_equal(out$adi_nat, expected$adi_nat)

  # Boundaries downloaded by state (states given, then worked out locally)
  out_states <- geo_link(places, lat = "lat", lon = "lon", states = places$state,
                         indices = indices)
  expect_equal(out_states$geoid_block, expected$geoid_block)
  expect_message(out_auto <- geo_link(places, lat = "lat", lon = "lon", indices = indices),
                 "block groups for: AL, AZ, CA, CO, CT, GA, IL, OR, PA, TX")
  expect_equal(out_auto$geoid_block, expected$geoid_block)
})

test_that("drop_identifiers = TRUE removes address, coordinate, and GEOID columns", {
  d <- places %>%
    mutate(street = "x", city = "y", zip = "00000", county_name = "z")
  expect_message(
    out <- geo_link(d, lat = "lat", lon = "lon", bg_shapes = example_bg, indices = indices,
                    drop_identifiers = TRUE),
    "Dropped identifier"
  )
  expect_false(any(c("lat", "lon", "street", "city", "zip", "county_name",
                     "geoid_block", "geoid_tract") %in% names(out)))
  expect_true(all(c("id", "state", "svi_tract", "adi_nat", "z_COI_nat") %in% names(out)))
})

test_that("geo_2020() refuses to call the Census geocoder without opt-in", {
  expect_error(geo_2020(example_addresses), "allow_external_geocoder = TRUE")
})
