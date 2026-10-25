# Test geo_2020() on the ten public state capitols (test/example_data.R)
# Run from the repo root: Rscript test/test_geo_2020.R
#
# Note: geo_2020() sends the addresses to the U.S. Census Bureau geocoder (so it
# needs allow_external_geocoder = TRUE), and the download = TRUE run reads the
# index data from GitHub, so this test needs internet access. The
# download = FALSE run reads the repo's local Data/ folder.

library(testthat)

source("src/geo_link.R")
source("src/geo_2020.R")
source("test/example_data.R")

x <- example_addresses

result <- geo_2020(data = x, download = TRUE, coi_year = 2023,
                   allow_external_geocoder = TRUE)

print(as.data.frame(result))

test_that("geo_2020 refuses to geocode without opt-in", {
  expect_error(geo_2020(data = x), "allow_external_geocoder = TRUE")
})

test_that("geo_2020 returns one row per address, in order", {
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(x))
  expect_equal(result$street, x$street)
  expect_equal(result$state, x$state)
})

test_that("addresses geocode to the expected locations", {
  # Within about 100 m of the coordinates recorded in example_data.R
  expect_true(all(abs(result$cxy_lat - example_coords$lat) < 0.001))
  expect_true(all(abs(result$cxy_lon - example_coords$lon) < 0.001))
})

test_that("census GEOIDs are well formed and match the expected block groups", {
  expect_equal(result$geoid_block, example_expected$geoid_block)
  expect_equal(result$geoid_tract, substr(example_expected$geoid_block, 1, 11))
  expect_true(all(nchar(result$geoid_block) == 12))
})

test_that("index values are joined and in valid ranges", {
  expect_equal(result$svi_tract, example_expected$svi_tract)
  expect_equal(result$adi_nat, example_expected$adi_nat)
  expect_equal(result$adi_state, example_expected$adi_state)
  expect_true(all(result$svi_block >= 0 & result$svi_block <= 1))
  expect_false(anyNA(result$z_COI_nat))
})

result_local <- geo_2020(data = x, download = FALSE, coi_year = 2023,
                         allow_external_geocoder = TRUE)

test_that("local Data/ files give the same result as the GitHub download", {
  expect_equal(result_local, result)
})
