# Align Columns
# Patrick Tu
# Started on 7.18.24

align_columns <- function(df1, df2) {
  # Get column names from both data frames
  cols1 <- colnames(df1)
  cols2 <- colnames(df2)
  
  # Find columns that are in df1 but not in df2
  cols_to_add_to_df2 <- setdiff(cols1, cols2)
  # Find columns that are in df2 but not in df1
  cols_to_add_to_df1 <- setdiff(cols2, cols1)
  
  # Add missing columns to df2 and fill with NA
  for (col in cols_to_add_to_df2) {
    df2[[col]] <- NA
  }
  
  # Add missing columns to df1 and fill with NA
  for (col in cols_to_add_to_df1) {
    df1[[col]] <- NA
  }
  
  # Ensure the columns are in the same order
  df1 <- df1[, sort(colnames(df1))]
  df2 <- df2[, sort(colnames(df2))]
  
  return(list(df1 = df1, df2 = df2))
}
