set_error <- function(obj){
  obj$attributes$error <- TRUE
  obj
}

bind_attributes <- function(obj,b = NULL){
  if(!is.null(b)){
    a <- obj$attributes
    if(!is.null(a)){
      obj$attributes <- c(a, b)
    } else {
      obj$attributes <- b
    }
  }
  obj
}

collect_attributes <- function(series){
  do.call("data.frame", series$attributes)
}