# Regression check: the old geo_2020() (commit 0b9966e) vs. the new path
#   Census lat/lon -> assign_geoids() -> geo_link()
# on the ten public state capitols (test/example_data.R). GEOIDs and every index
# value should be identical, except Connecticut: the old function returned NA
# for its SVI tract and ADI values (planning-region codes), the new one fills them.
# Run from the repo root (needs git and internet): Rscript test/regression_geo_2020.R
#
# Both runs send these PUBLIC example addresses to the Census geocoder. Never
# run this on participant data.

library(testthat)
library(tibble)

source("test/example_data.R")
x <- example_addresses

# Old function, straight from git history, in its own environment
old_file <- tempfile(fileext = ".R")
system2("git", c("show", "0b9966e:src/geo_2020.R"), stdout = old_file)
old_env <- new.env()
sys.source(old_file, envir = old_env)
old <- old_env$geo_2020(data = x, download = FALSE, coi_year = 2023)

# New functions
source("src/geo_link.R")
source("src/geo_2020.R")

# New path: only the coordinates from the old run are reused, and block groups
# are assigned locally
new <- geo_link(select(old, street:zip, cxy_lon, cxy_lat),
                lat = "cxy_lat", lon = "cxy_lon", states = x$state,
                download = FALSE, data_dir = "Data")

# The opt-in wrapper should reproduce the old output exactly
wrapped <- geo_2020(x, download = FALSE, coi_year = 2023, allow_external_geocoder = TRUE)

index_cols <- c("svi_tract", "svi_block", "adi_nat", "adi_state",
                grep("^z_.*_nat$", names(old), value = TRUE))
ct <- x$state == "CT"
ct_fixed <- c("svi_tract", "adi_nat", "adi_state")

test_that("local point-in-polygon gives the same GEOIDs as the Census geocoder", {
  expect_equal(new$geoid_block, old$geoid_block)
  expect_equal(new$geoid_tract, old$geoid_tract)
})

test_that("index values are identical outside Connecticut", {
  expect_equal(as.data.frame(new[!ct, index_cols]), as.data.frame(old[!ct, index_cols]))
})

test_that("Connecticut: old function gave NA, new path fills SVI tract and ADI", {
  expect_true(all(is.na(unlist(old[ct, ct_fixed]))))
  expect_false(anyNA(unlist(new[ct, ct_fixed])))
  # Everything else for Connecticut is unchanged
  other <- setdiff(index_cols, ct_fixed)
  expect_equal(as.data.frame(new[ct, other]), as.data.frame(old[ct, other]))
})

test_that("geo_2020(allow_external_geocoder = TRUE) matches the old output", {
  expect_equal(as.data.frame(wrapped[!ct, ]), as.data.frame(old[!ct, ]))
  expect_equal(as.data.frame(wrapped[ct, ct_fixed]), as.data.frame(new[ct, ct_fixed]))
})
