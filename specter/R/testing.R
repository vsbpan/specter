rand_drop <- function(series, n = 10, p = NULL, set_as_NA = TRUE){
  if(!is.null(p)){
    n <- ceiling(length(series$x)*p)
  }
  if(n == 0){
    return(series)
  }
  
  i <- sample(series$x, size = n)
  if(set_as_NA){
    series$y[i] <- NA_real_
  } else {
    series$x <- series$x[-i]
    series$y <- series$y[-i]
  }
  
  series
}
