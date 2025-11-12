
#' Parse a read file
#'
#' This function parses a read file from a spectrophotometer
#'
#' @param file Path to the input file
#' @param mthd the parsing method to be used
#' @export
parse_reads <- function(file,mthd=biotek_ten_wavelengths_universal){

  # apply a parsing method to a particular file
  wv_data <- mthd(file)

  parsed_data <- add_well_plate_exp_maps(wv_data)

  return(parsed_data)
}



#' helper for the ten_wavelengths.prt protocol
#'
#' This function parses a read file from a spectrophotometer
#'
#' @param file Path to the input file
#'
#' @export
biotek_ten_wavelengths_universal <- function(file){
  p_rows <- 16
  p_cols <- 24
  wavelengths <-  c(510,520,530,550,560,570,610,620,630,700)
  n <- length(wavelengths)
  wv_names = wavelengths |> {function(x) paste("wv",x,sep="")}() # pipe the contents of wavelengths into an anonymous function to create wavelength names

  raw_abs <- openxlsx::read.xlsx(file, rows = 39:199, cols = 3:26, colNames=TRUE) # change depending on file format

  #### function that parses out each well plate row of data
  clean_abs <- data.frame()
  for (j in 1:p_cols)
      for (i in 1:p_rows) {
      # print(i)
      row <- raw_abs[(1 + ( n*(i - 1))): (n*i),j ]
      clean_abs <- rbind(clean_abs,row)
  }

  # # browser()
  ## check for correct dimensions
  n_rows <- nrow(clean_abs)
  n_cols <- ncol(clean_abs)
  if (n_rows != 384 | n_cols != n){
    stop(paste0("Data parsing not correct dimensions (384 rows x ", n," cols) \n"))
    }


  ### parsed out absorbances

  ## transposing the data where the columns are the wavelengths_510_700 ## can't combine until data is excluded

  colnames(clean_abs) <-wv_names
  return(clean_abs)
}

#' helper for the 300:700 nm spectra biotek read protocol
#'
#' This function parses a read file from a spectrophotometer
#'
#' @param file Path to the input file
#' @export
biotek_300_700_universal <- function(file){
  p_rows <- 16
  p_cols <- 24
  wavelengths <-  seq(300,700,10)
  n <- length(wavelengths)
  wv_names = wavelengths |> {function(x) paste("wv",x,sep="")}() # pipe the contents of wavelengths into an anonymous function to create wavelength names
  rows <- list(c(25:(25+n)),c(70:(70+n)),c(115:(115+n)),c(160:(160+n)))
  raw_abs = data.frame(matrix(NA, nrow=41,ncol=1))
  for (i in rows){
    abs <-  openxlsx::read.xlsx(file, rows = i, cols = 3:(3+96), colNames=TRUE)
    raw_abs=cbind(raw_abs,abs)
  }
  clean_abs = as.data.frame(t(raw_abs[,-1]))


  # #### function that parses out each well plate row of data
  # clean_abs <- data.frame()
  # for (j in 1:p_cols)
  #   for (i in 1:p_rows) {
  #     # print(i)
  #     row <- raw_abs[(1 + ( n*(i - 1))): (n*i),j ]
  #     clean_abs <- rbind(clean_abs,row)
  #   }

  # # browser()
  ## check for correct dimensions
  n_rows <- nrow(clean_abs)
  n_cols <- ncol(clean_abs)
  if (n_rows != 384 | n_cols != n){
    stop(paste0("Data parsing not correct dimensions (384 rows x ", n," cols) \n"))
  }


  ### parsed out absorbances

  ## transposing the data where the columns are the wavelengths_510_700 ## can't combine until data is excluded

  colnames(clean_abs) <-wv_names
  return(clean_abs)
}









# ### generating dye experimental map ###



## generating sample experimental map (this assumes all wells are filled)
## this is for production ready for blank+universal layout. plate 96 well plates per 384 well plate


### 96 well plate ID's using numbered indices
### Creating indexes as numbers 1 - 96




### functions that adds 96 well and 384 well indexes both letter and number
add_well_plate_exp_maps <- function(data){

  well_96_num <- c()
  well_96_index <- c()
  for (i in 1:12) {
    row_num <- c(1:8) + 8 * (i - 1)
    row_num <- rep(row_num, times=2, each=2)
    letter_vector <- paste0(LETTERS[1:8], i)
    letter_vector <- rep(letter_vector, times=2, each=2)
    well_96_num <- c(well_96_num, row_num)
    well_96_index <- c(well_96_index,letter_vector)
  }


  ### Creating indexes as 1-384. This will allow for easy sorting for 384-well format
  well_384_num <- c(1:384)

  ### Creating indices as A1-A24... P1-P24 for 384-well format
  well_384_index <- c()
  row_let_384 <- LETTERS[1:16]
  for (i in 1:24) {
    letter_vector <- paste0(row_let_384, i)
    well_384_index <- c(well_384_index, letter_vector)
  }

  plate_number <- rep(c(1, 2), times = 192)
  ### adds 96 well numeric indices from "make_exp_maps.R"
  data$WP96_idx <- well_96_num

  ### adds 96 well row-colmun indices from "make_exp_maps.R"
  data$WP96 <- well_96_index
  # print(well_96_index)

  ### adds 384 well numeric indices from "make_exp_maps.R"
  data$WP384_idx <- well_384_num

  ### adds 384 well row-colmun indices from "make_exp_maps.R"
  data$WP384 <- well_384_index
  # print(well_384_index)

  data$plate <- plate_number
  dyes_names1 <- c("blank", "universal")

  dyes_names1_rep <-rep(dyes_names1, each=16)
  dye_names_exp_map <- rep(dyes_names1_rep, times=12)
  data$dye <- dye_names_exp_map

  return(data)
}



