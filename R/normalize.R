


magnitude <- function(wv_data,...) {
  row_magnitudes <- apply(wv_data, MARGIN=1, FUN= function(row){
    sqrt(sum(row^2))
  })
  return(row_magnitudes)
}


reference_wavelength <- function(wv_data,reference_name="wv700",...){
  return(wv_data[,reference_name])
}



### function that normalizes a dataframe by each row.
normalize <- function(data,norm_mthd=magnitude,wavelength_keyword="wv",...) {

  factor <- norm_mthd(data,...)
 new_data <- data / factor
  return(new_data)
}
