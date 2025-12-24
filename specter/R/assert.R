has_error <- function(obj){
  isTRUE(obj$attributes$error)
}

is.SpatRaster <- function(x){
  inherits(x,"SpatRaster")
}

# Check object class
is.mle_fit <- function(x, ...){
  inherits(x, "mle_fit")
}

is.map_fit <- function(x, ...){
  inherits(x, "map_fit")
}

is.series <- function(x, ...){
  inherits(x, "series")
}