# Checking to see Downloaded Packages
# Patrick Tu
# 4.21.24

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