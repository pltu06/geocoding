# Hex Testing
# Patrick Tu
# 7.23.24

library(greenR)
library(osmdata)

data <- get_osm_data("Evanston, IL")
green_areas_data <- data$green_areas
tree_data <- data$trees

# Generate the visualization
hex_map <- hexGreenSpace(green_areas_data, tree_data, hex_size = 1000, color_palette = "viridis")

# Display the hex bin map and the statistical violin plot
print(hex_map$map)  # Display the map
print(hex_map$violin)  # Display the violin plot