#!/usr/bin/env bash
# Geocode the ten public state capitols with DeGAUSS, entirely on this machine,
# then check the result with geo_link().
# Run from the repo root: bash test/degauss/run_degauss.sh
#
# Needs Docker (Docker Desktop or Colima). The first run downloads the DeGAUSS
# images (the geocoder image is several GB); after that nothing is downloaded.
# The DeGAUSS output CSVs are written next to capitols.csv and are git-ignored.

set -euo pipefail

# Pinned versions. Check https://degauss.org (and the tags at
# https://github.com/orgs/degauss-org/packages) before changing them.
GEOCODER=ghcr.io/degauss-org/geocoder:3.4.1
BLOCK_GROUP=ghcr.io/degauss-org/census_block_group:1.1.1

cd "$(dirname "$0")"

# Clear output from earlier runs so the file lookups below are unambiguous
rm -f capitols_*.csv

# 1. Geocode addresses -> lat/lon
docker run --rm -v "$PWD":/tmp "$GEOCODER" capitols.csv

# DeGAUSS puts its version in the output file name; find it rather than guess
geocoded=$(ls capitols_geocoder_*_score_threshold_*.csv)
echo "Geocoder output: $geocoded"

# 2. Add 2020 census block group and tract IDs
docker run --rm -v "$PWD":/tmp "$BLOCK_GROUP" "$geocoded" 2020

cd ../..

# 3. Link indices with geo_link() and compare to test/example_data.R
Rscript test/degauss/check_degauss.R
