set_error <- function(obj, reason = character(0)){
  obj$attributes$error <- TRUE
  if(length(obj$attributes$error_reason) > 0){
    obj$attributes$error_reason <- paste(obj$attributes$error_reason, reason, sep = ";") 
  } else {
    obj$attributes$error_reason <- reason
  }
  
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
  series$attributes <- lapply(series$attributes, function(x){
    if(length(x) == 0){
      return(NA)
    } else {
      x
    }
  })
  do.call("data.frame", series$attributes)
}