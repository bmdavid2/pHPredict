
norm_pH <- function(pH_vals){
  
  return((pH_vals-7)/7)
}





#' Train a model for pH prediction 
#' 
#' Use absorbance data and measured pH values to train a model.
#' 
#' @param abs_data a parsed absorbance dataframe 
#' @param pH_vals pH values corresponding to each row of absorbance values 
#' @param mdl_generator a generator function that produces a trained model and returns a prediction function, currently `gp_model` and `linear_model` are supported
#' @export 
train <- function(abs_data, pH_vals, mdl_generator ,wavelength_keyword="wv",...){
  wv_cols  <- grepl(wavelength_keyword,names(abs_data))
  abs <- abs_data[,wv_cols]
  n_pH <- norm_pH(pH_vals)
  pred_fun <- mdl_generator(abs,n_pH,...)
  
  return(pred_fun)
}



