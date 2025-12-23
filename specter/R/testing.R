series_rand_drop <- function(series, n = 10, p = NULL, set_as_NA = FALSE){
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


pred_error_score <- function(pred_freq, pred_power, target){
  keep <- !is.na(pred_power) & !is.na(pred_freq)
  pred_freq <- pred_freq[keep]
  pred_power <- pred_power[keep]
  
  
  
  if(length(pred_freq) < 2){
    return(NA_real_)
  }
  # Log difference
  log(mean_freq(pred_freq, pred_power)) - log(target)
}
