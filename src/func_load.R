# Checking to see Downloaded Packages
# Patrick Tu
# 4.21.24
#
# Note: newer code (src/geo_link.R) does not install packages for you; it
# tells you which ones to install. Please don't copy this pattern into functions.

packages <- c("remotes", "readr", "dplyr", "tidyr")

for (i in 1:length(packages)) {
  req <- require(packages[i], character.only = TRUE)
  
  if (req) {
    library(packages[i], character.only = TRUE)
  } else {
    install.packages(packages[i])
    
    library(packages[i], character.only = TRUE)
  }
}

test <- require("censusxy", character.only = TRUE)

if (test) {
  library(censusxy)
} else {
  remotes::install_github("chris-prener/censusxy")
  
  library(censusxy)
}

# Need to add censusxy functionality