

predict <- function(data, mdl){
  pred <- mdl(data)
  return(cbind(data,pred))

}


