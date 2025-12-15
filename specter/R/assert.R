has_error <- function(obj){
  isTRUE(obj$attributes$error)
}

is.SpatRaster <- function(x){
  inherits(x,"SpatRaster")
}