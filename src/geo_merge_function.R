# Function merges COI and SVI function data into one dataset

geo_merge_function <- function(data = x, col_merge = "street"){
  dat1 <- coi_sdi_function(data = data)
  
  dat2 <- svi_adi_function(data = data)
  
  dat3 <- left_join(dat1, dat2, by = col_merge)
  
  return(dat3)
}
