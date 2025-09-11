quality_control <- function(df,...){
  pass_background_subtract <- check_background_subtr(df,...)
  df$PassBackground <- pass_background_subtract
  
  return(df)
}





check_background_subtr <- function(df, subtraction_threshold=-0.1, wavelength_keyword="wv") {
  # Check the number of rows (assumes all dataframes are the same length)
  wv_cols  <- grepl(wavelength_keyword,names(data))
  
  
  cat("Check for absorbances below threshold value: ", subtraction_threshold, "\n")
  
  n_rows <- nrow(df)
  # Build a logical vector: TRUE = keep, FALSE = remove
  pass <- rep(TRUE, n_rows)
  
  # Loop through rows
  for (i in 1:n_rows) {
    # For each dataframe, extract row i (as numeric)
    row_values <- as.numeric(df[i,wv_cols])
    # row_values is a list or matrix of row values, flatten to vector
    all_row_values <- unlist(row_values)
    # If any value in this row across dataframes fails the condition...
    if (any(all_row_values <= subtraction_threshold)) {
      pass[i] <- FALSE
      cat("Well :", df[i, 'WP384'], "is flagged with values < -0.1 \n")
    }
  }
  
  # Return the list of dataframes with only the rows we want to keep
  return(pass)
  
  
}