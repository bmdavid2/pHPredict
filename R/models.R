
# model functions take absorbance and pH data as input and return a function that makes a prediction using the trained model with new data as inputs

norm_pH <- function(pH_vals){
  
  return((pH_vals-7)/7)
}



#' Train a GP model for making pH predictions 
#' 
#' This function does not include hyperparameter optimization and assumes that all absorbance inputs and pH measurements are normalized
#' 
#' @param abs a n x k dataframe of absorbance measurements, where n is the number of observations, and k is the number of wavelengths 
#' @param ph a normalized vector of length n of pH measurements for training 
#' @return a generated function that makes a prediction with the model with new data as an input 
#' @export 
gp_model <- function(abs,ph){
  # transform ph values to a -1 to 1 scale (helpful for stock gp training) 
  ph_data <- norm_pH(ph)
  
  GP <- laGP::newGPsep(abs,ph_data,d=0.1,g=1e-6,dK=TRUE)

  
  pred_fun <- function(abs_data){
    preds <- laGP::predGPsep(GP,abs_data)
    
    # transform pH data back to normal scale 
    m <- 7*preds$mean +7 
    sd <- 7* sqrt(diag(preds$Sigma))
    out_df <- data.frame(mean_pH = m , sd = sd )
    return(out_df )
  }
  return(pred_fun)
}

#' Train a linear model with First order and Two way interaction terms 
#' 
#' pH ~ FO(abs) + TWI(abs) 
#' 
#'
#' This function does not include hyperparameter optimization and assumes that all absorbance inputs and pH measurements are normalized
#' 
#' @param abs a n x k data frame of absorbance measurements, where n is the number of observations, and k is the number of wavelengths 
#' @param ph a normalized vector of length n of pH measurements for training 
#' @return a generated function that makes a prediction with the model with new data as an input 
#' @export 
FO_TWI_lin_model <- function(abs,pH){
  # to be implemented 
  mdl <- pH ~rsm::FO(abs) +rsm::TWI(abs) 
  
  
  pred_fun <- function(abs_data){
    preds <- predict.lm(mdl,abs_data)
    
    # transform pH data back to normal scale 
    out_df <- data.frame(mean_pH = preds)
    return(out_df )
  }
  return(pred_fun) 
  
}

#' Train a linear model with First order and Two way interaction and pure quadratic terms 
#' 
#' pH ~ rsm::SO(abs)
#' 
#'
#'
#' 
#' @param abs a n x k data frame of absorbance measurements, where n is the number of observations, and k is the number of wavelengths 
#' @param ph a normalized vector of length n of pH measurements for training 
#' @return a generated function that makes a prediction with the model with new data as an input 
#' @export 
SO_lin_model <- function(abs,pH){
  # to be implemented 
  mdl <- pH ~rsm::SO(abs)
  
  pred_fun <- function(abs_data){
    preds <- predict.lm(mdl,abs_data)
    
    # transform pH data back to normal scale 
    out_df <- data.frame(mean_pH = preds )
    return(out_df )
  }
  return(pred_fun) 
  
}
  
  