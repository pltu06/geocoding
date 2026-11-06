# Check DeGAUSS output for the ten public state capitols against the values in
# test/example_data.R (which came from the Census geocoder).
# Run from the repo root after the DeGAUSS steps in run_degauss.sh.
#
# DeGAUSS uses its own street database, so its coordinates differ slightly from
# the Census geocoder's. A capitol can land in a neighboring block group, so a
# mismatch here is worth a look but isn't necessarily a bug.

library(testthat)

source("src/geo_link.R")
source("test/example_data.R")

# DeGAUSS names its output after the container versions, so look it up
out_file <- list.files("test/degauss", pattern = "^capitols_geocoder_.*_census_block_group_.*_2020\\.csv$",
                       full.names = TRUE)
stopifnot("Expected exactly one DeGAUSS output file; run run_degauss.sh first" =
            length(out_file) == 1)
message("Reading ", out_file)

d <- read_csv(out_file, show_col_types = FALSE) %>%
  arrange(id)  # line rows up with example_data.R in case DeGAUSS reorders them

# Show what DeGAUSS returned
print(as.data.frame(select(d, id, matched_street, precision, score, lat, lon,
                           census_block_group_id_2020)))

out <- geo_link(d, geoid_block = "census_block_group_id_2020",
                download = FALSE, data_dir = "Data")

test_that("DeGAUSS geocoded every capitol precisely", {
  expect_equal(nrow(d), 10)
  expect_true(all(d$geocode_result == "geocoded"))
  expect_true(all(d$precision %in% c("range", "street")))
})

test_that("DeGAUSS coordinates are within about 500 m of the Census geocoder's", {
  km <- 6371 * sqrt(((d$lat - example_coords$lat) * pi / 180)^2 +
                      ((d$lon - example_coords$lon) * pi / 180 *
                         cos(example_coords$lat * pi / 180))^2)
  print(data.frame(state = example_coords$state, km_from_census = round(km, 3)))
  expect_true(all(km < 0.5))
})

test_that("geo_link() reads DeGAUSS block group IDs, including leading zeros", {
  # read_csv may read census_block_group_id_2020 as a number; geo_link() pads it
  expect_true(all(nchar(out$geoid_block) == 12))
  mismatch <- out$geoid_block != example_expected$geoid_block
  if (any(mismatch)) {
    print(data.frame(state = example_coords$state[mismatch],
                     degauss = out$geoid_block[mismatch],
                     census = example_expected$geoid_block[mismatch]))
  }
  expect_equal(out$geoid_block, example_expected$geoid_block)
  expect_equal(out$svi_tract, example_expected$svi_tract)
  expect_equal(out$adi_nat, example_expected$adi_nat)
})

test_that("drop_identifiers = TRUE removes DeGAUSS address and location columns", {
  clean <- geo_link(d, geoid_block = "census_block_group_id_2020",
                    download = FALSE, data_dir = "Data", drop_identifiers = TRUE)
  print(names(clean))
  expect_false(any(c("address", "lat", "lon", "matched_street", "matched_zip",
                     "census_block_group_id_2020", "census_tract_id_2020",
                     "geoid_block", "geoid_tract") %in% names(clean)))
})
