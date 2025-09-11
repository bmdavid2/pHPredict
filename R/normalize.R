


magnitude <- function(wv_data) {
  row_magnitudes <- apply(wv_data, MARGIN=1, FUN= function(row){
    sqrt(sum(row^2))
  })
  return(row_magnitudes)
}


reference_wavelength <- function(wv_data,reference_name="wv700"){
  return(wv_data[,ref_name])
} 



### function that normalizes a dataframe by each row. 
normalize <- function(data,mthd=magnitude,wavelength_keyword="wv",...) {
  
  wv_cols  <- grepl(wavelength_keyword,names(data))
  
  
  factor <- mthd(data[ ,wv_cols],...)
  data[, wv_cols] <- data[ , wv_cols] / factor
  return(data)
}