# Bounding Box Testing
# Patrick Tu
# 7.18.24

library(tidycensus)
library(tigris)
library(dplyr)
library(osmdata)
library(sf)
library(greenR)

api_key <- "2b3886ed84d448af363b976fcea159633656da16"

census_api_key(key = api_key)

options(tigris_use_cache = TRUE)

cook_bg_geom <- get_acs(
  geography = "block group",
  variables = "B01002_001",
  state = "IL",
  county = "Cook",
  year = 2020,
  geometry = TRUE
)%>%
  mutate(tract_num = as.numeric(gsub("[^0-9.]", "", NAME)))%>%
  mutate(GEOID = as.numeric(GEOID))%>%
  filter(!st_is_empty(geometry))

bbox_cook <- vector("list", 4001)

for (i in 1:length(bbox_cook)) {
  bbox_cook[i] <- cook_bg_geom$geometry[[i]][[1]]
}

test <- as.matrix(cook_bg_geom$geometry[[1]][[1]][[1]])

query <- opq(bbox = test)

# Download landuse data
green_areas_data_landuse <- query %>%
  add_osm_feature(key = "landuse", 
                  value = c("forest", "vineyard", "plant_nursery", "orchard", 
                            "greenfield", "recreation_ground", "allotments", 
                            "meadow", "village_green", "flowerbed", "grass", 
                            "farmland"))%>%
  osmdata_sf()

# Download leisure data
green_areas_data_leisure <- query %>%
  add_osm_feature(key = "leisure", value = c("garden", "dog_park", "nature_reserve", "park")) %>%
  osmdata_sf()

# Initialize an empty list to hold the combined data
green_areas_data <- list()

# Align and combine 'sf' objects
aligned_polygons <- align_columns(df1 = green_areas_data_landuse$osm_polygons, df2 = green_areas_data_leisure$osm_polygons)

green_areas_data$osm_polygons <- rbind(aligned_polygons$df1, aligned_polygons$df2)

# Download highways data
highways_data <- query %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Download trees data
trees_data <- query %>%
  add_osm_feature(key = "natural", value = "tree") %>%
  osmdata_sf()

data <- list(highways = highways_data, green_areas = green_areas_data, trees = trees_data)

green_index <- calculate_green_index(data, 4326, 100)

evanston_green_index <- green_index%>%
  select(green_index)%>%
  summarise(green_index_neighborhood = mean(green_index))

plot <- plot_green_index(green_index, interactive = TRUE)