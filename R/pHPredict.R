
#' Predict the pH of wells in a microwell plate 
#' 
#' This function parses a read file from a spectrophotometer and builds a model using a supplied training data set.
#' The function makes a prediction using the model using the read file data as an input. 
#'
#' @param read_file Path to the input file 
#' @param model A function that takes parsed data as input and makes a prediction as an output
#' @export 
pHPredict <- function(read_file,model,wavelength_keyword="wv",dye_name="universal",...){
  



  # parse read file
  read_data <- parse_reads(read_file,...)
  wv_cols  <- grepl(wavelength_keyword,names(read_data))
  metadata <- read_data[,!wv_cols]
  # background subtraction 
  bgsub_data <- subtract_background(read_data,dye_name)
  # run qc 
  qc_data <- quality_control(bgsub_data,...)
  # normalize data 
  norm_data <- normalize(qc_data,...)
  
  preds <- predict(norm_data,model,...) 
  
  return(cbind(preds,metadata,qc_data$PassQC))
  
} 
  
  
  
  
  