

predict <- function(data, mdl_obj){
  mdl <- mdl_obj[1]
  wv_cols <- mdl_obj[2]
  #wv_cols  <- grepl(wavelength_keyword,names(data))

  abs_data <- data[,wv_cols]
  pred <- mdl(abs_data)
  return(cbind(data,pred))

}


