# Check that geo_link() runs with the network blocked, i.e. that the default
# path never needs to send anything off the machine.
# Run from the repo root: Rscript test/test_offline.R
#
# Step 1 (online): download block group boundaries for the ten example states
#   (only the state codes are sent) and save them to a temporary GeoPackage.
# Step 2 (offline): run geo_link() in a separate R process whose HTTP(S) proxy
#   points at a closed local port, so any web request fails. Index data come
#   from Data/ (download = FALSE) and boundaries from the GeoPackage.
#
# The proxy trick blocks R's web tools (curl, httr, download.file, readr URLs).
# For a stricter check, turn off Wi-Fi / unplug the network and run step 2 alone.

library(testthat)

source("test/example_data.R")

bg_file <- file.path(tempdir(), "example_block_groups_2020.gpkg")
options(tigris_use_cache = TRUE)
example_bg <- do.call(rbind, lapply(example_coords$state, function(s) {
  tigris::block_groups(state = s, year = 2020, progress_bar = FALSE)
}))
sf::st_write(example_bg, bg_file, quiet = TRUE, delete_dsn = TRUE)

offline_script <- file.path(tempdir(), "offline_run.R")
writeLines(c(
  'suppressMessages(source("src/geo_link.R"))',
  'stopifnot(Sys.getenv("https_proxy") == "http://127.0.0.1:9")',
  # Sanity check: the network really is blocked in this process
  'blocked <- tryCatch({ download.file("https://raw.githubusercontent.com/pltu06/geocoding/main/README.md",',
  '                                     tempfile(), quiet = TRUE); FALSE },',
  '                    error = function(e) TRUE, warning = function(w) TRUE)',
  'stopifnot("network was not blocked" = blocked)',
  'source("test/example_data.R")',
  'd <- example_coords',
  sprintf('out <- geo_link(d, lat = "lat", lon = "lon", bg_shapes = "%s",', bg_file),
  '                download = FALSE, data_dir = "Data")',
  'stopifnot(identical(out$geoid_block, example_expected$geoid_block))',
  'stopifnot(identical(out$adi_nat, example_expected$adi_nat))',
  'stopifnot(!anyNA(out$svi_tract), !anyNA(out$z_COI_nat))',
  'cat("OFFLINE_OK\\n")'
), offline_script)

dead_proxy <- "http://127.0.0.1:9"
proxy_env <- c(paste0(c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY",
                        "ALL_PROXY", "all_proxy"), "=", dead_proxy),
               "no_proxy=", "NO_PROXY=")
result <- system2(file.path(R.home("bin"), "Rscript"), offline_script,
                  env = proxy_env, stdout = TRUE, stderr = TRUE)
cat(result, sep = "\n")

test_that("geo_link() works with the network blocked", {
  expect_true("OFFLINE_OK" %in% result)
})
