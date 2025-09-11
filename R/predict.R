

predict <- function(data, mdl,wavelength_keyword="wv"){
  
  wv_cols  <- grepl(wavelength_keyword,names(data))
  
  abs_data <- data[,wv_cols]
  pred <- mdl(abs_data)
  return(cbind(data,pred))
  
}
  
  
  