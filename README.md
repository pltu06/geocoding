# geocoding

R functions that link participant locations to neighborhood indices:

- **SVI** (CDC Social Vulnerability Index): 2022 tract and 2020 block group
- **ADI** (Area Deprivation Index): 2023 block group, national and state ranks
- **COI 3.0** (Child Opportunity Index): 2012–2023 tract, overall z-score, 3 domains, 14 subdomains
- **SDI** (Social Deprivation Index): 2019, 2010 tracts (`geo_2010()` only)

**How to use the functions:** see the step-by-step guide at <https://pltu06.github.io/geocoding/>.

## Privacy

The main function, `geo_link()`, takes **census IDs (GEOIDs) or latitude/longitude**, not addresses. On its default path, **this code does not transmit participant data off the user's machine and can be used within a HIPAA-compliant workflow.** Addresses, coordinates, and GEOIDs never leave your computer. Whether a workflow is HIPAA compliant depends on your institution and its procedures, so consult your IRB and privacy office.

Under HIPAA Safe Harbor, coordinates and any geography smaller than a state (county, tract, block group) are identifiers too. This code works with them locally, and only ever *downloads* public data:

| Network call | When | What is sent |
|---|---|---|
| Index CSVs from `raw.githubusercontent.com/pltu06/geocoding` | `download = TRUE` (default) | Nothing about your data. Use `download = FALSE` + `data_dir` to read a local copy of `Data/`. |
| TIGER/Line boundary files via `tigris` | `geo_link(lat =, lon =)` without `bg_shapes` | Only a state code (e.g. `"IL"`). Pass a local file with `bg_shapes` to stay fully offline. |
| U.S. Census Bureau geocoder via `censusxy` | **Only** `geo_2020()` / `geo_2010()` with `allow_external_geocoder = TRUE` | **Full addresses.** For non-sensitive data only. |

`src/bound_box.R` (`get_osm_data()`) queries the public Nominatim service with a *place name* you type, not participant data.

## Quick start

### 1. Get GEOIDs or coordinates with a tool your institution approves

We recommend [DeGAUSS](https://degauss.org), which geocodes on your own computer with Docker (the images are downloaded once; addresses are not sent anywhere). Your input CSV needs an `address` column (e.g. `"401 S 2nd St Springfield IL 62701"`).

Give Docker at least 8 GB of memory: the block group step needs 5 GB and stops if it doesn't have it. With Docker Desktop, set this under Settings → Resources; with [Colima](https://github.com/abiosoft/colima), start it with `colima start --memory 8`.

```sh
# Geocode addresses locally
docker run --rm -v "$PWD":/tmp ghcr.io/degauss-org/geocoder:3.4.1 addresses.csv
# Add 2020 block group and tract IDs
docker run --rm -v "$PWD":/tmp ghcr.io/degauss-org/census_block_group:1.1.1 \
  addresses_geocoder_3.4.1_score_threshold_0.5.csv 2020
```

(Check <https://degauss.org> for current versions. The DeGAUSS `geocoder` image is in maintenance mode; see <https://degauss.org/migrate/geocoder> for its successor.)

Other options: ArcGIS desktop, your institution's own geocoder, or a geocoding service under a business associate agreement (BAA), e.g. Geocodio's HIPAA option. Any of these can give you latitude/longitude.

### 2. Link the indices in R

```r
install.packages(c("dplyr", "readr", "stringr", "tidyr", "sf", "tigris"))
source("https://raw.githubusercontent.com/pltu06/geocoding/main/src/geo_link.R")

# From DeGAUSS block group IDs
d   <- readr::read_csv("addresses_geocoder_3.4.1_score_threshold_0.5_census_block_group_1.1.1_2020.csv")
out <- geo_link(d, geoid_block = "census_block_group_id_2020", drop_identifiers = TRUE)

# From tract IDs only (block group indices SVI-bg and ADI will be NA)
out <- geo_link(d, geoid_tract = "census_tract_id_2020")

# From coordinates (block groups are assigned locally with sf)
out <- geo_link(d, lat = "lat", lon = "lon", states = "IL")
```

`geo_link()` adds `geoid_block`, `geoid_tract`, `svi_tract`, `svi_block`, `adi_nat`, `adi_state`, and the 18 `z_*_nat` COI columns. Use `coi_year` (2012–2023) to pick the COI year. GEOIDs must be **2020** boundaries; leading zeros dropped by spreadsheet software are restored automatically, and Connecticut GEOIDs that use the 2022 planning regions are converted to the 2020 county codes. `drop_identifiers = TRUE` removes address, coordinate, county, and GEOID columns from the output (check the rest of your columns yourself).

### Fully offline

```r
# Once, on a computer with internet: save boundaries for your states
sf::st_write(tigris::block_groups("IL", year = 2020), "il_bg_2020.gpkg")
# Then, offline, with a local copy of this repo's Data/ folder
out <- geo_link(d, lat = "lat", lon = "lon", bg_shapes = "il_bg_2020.gpkg",
                download = FALSE, data_dir = "Data")
```

### Census geocoder (opt-in, non-sensitive data only)

`geo_2020()` still geocodes addresses (`street`, `city`, `state`, `zip` columns) with the U.S. Census Bureau geocoder, which is a third party with no BAA. It only runs if you opt in:

```r
source("https://raw.githubusercontent.com/pltu06/geocoding/main/src/geo_link.R")
source("https://raw.githubusercontent.com/pltu06/geocoding/main/src/geo_2020.R")
out <- geo_2020(x, allow_external_geocoder = TRUE)
```

See the [how-to guide](https://pltu06.github.io/geocoding/) for a worked example.

## Tests

Run from the repo root: `Rscript test/test_geo_link.R`, `Rscript test/test_offline.R` (runs `geo_link()` with the network blocked), `Rscript test/test_geo_2020.R` and `Rscript test/regression_geo_2020.R` (these two send public example addresses to the Census geocoder), and `bash test/degauss/run_degauss.sh` (geocodes the example addresses locally with DeGAUSS; needs Docker).

## Data sources

SVI block group dataset 
2020 Bryan, Michael, 2022, "US Social Vulnerability by Census Block Groups", <https://doi.org/10.7910/DVN/ARBHPK>, Harvard Dataverse, V2, UNF:6:sM/cBUxMDjFYmAdIA/dWBg== [fileUNF]

COI 3.0 overall index and three sub-domains 
diversitydatakids.org. 2024. “Child Opportunity Index 3.0 database, 2020 census tracts”, retrieved from <https://www.diversitydatakids.org/research-library/child-opportunity-index/child-opportunity-index-30-2023-census-tract-data> on Sep 17 2025.

SVI census tract dataset 2022 
Centers for Disease Control and Prevention/ Agency for Toxic Substances and Disease Registry/ Geospatial Research, Analysis, and Services Program. CDC/ATSDR Social Vulnerability Index, 2022, Database, U.S. <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>. Accessed on Jun 06 2024.

ADI census block group dataset 2020-2021 
University of Wisconsin School of Medicine and Public Health. 2020-2021 Area Deprivation Index National & State. Downloaded from <https://www.neighborhoodatlas.medicine.wisc.edu/> on Jun 06 2024.

SDI census tract dataset 2019
Social deprivation index (SDI). Robert Graham Center - Policy Studies in Family Medicine & Primary Care. (2019, November 5). Retrieved Jun 06, 2024, from <https://www.graham-center.org/rgc/maps-data-tools/sdi/social-deprivation-index.html>. 

## HIPAA compliance development

Tracking issue: <https://github.com/pltu06/geocoding/issues/4>.
