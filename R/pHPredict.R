
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

  # background subtraction
  bg_sub_data <- subtract_background(read_data,dye_name,...)
  metadata <- bg_sub_data[,!wv_cols]
  bg_sub_abs <- bg_sub_data[, c(model[[2]])]


  # run qc
  qc<- quality_control(bg_sub_abs,...)
  # normalize data
  norm_abs <- normalize(bg_sub_abs,...)
  # only wavelengths used for the model
  preds <- predict(norm_abs,model[[1]]) # actual model function

  return(cbind(preds,metadata,qc))

}




