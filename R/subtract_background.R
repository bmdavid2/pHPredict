

subtract_background <- function(data,dye_name,wavelength_keyword="wv",...){
  wv_cols  <- grepl(wavelength_keyword,names(data))
dye_data <- data[which(data$dye == dye_name),]
blank_data <- data[which(data$dye == "blank"),]
dye_metadata <- data[which(data$dye==dye_name),!wv_cols]


sub_data <- dye_data[,wv_cols] - blank_data[,wv_cols]

return(cbind(sub_data,dye_metadata))

}


