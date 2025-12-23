series_format <- function(series){
  series <- series_gapfill(series)
  lN <- log(series$data$y)
  out <- data.frame(
    "lN" = lN,
    "lN_lag" = dplyr::lag(lN, n = 1)
  )
  out <- out[!is.na(out$lN) & !is.na(out$lN_lag),]
  out
}

ricker_auto_start <- function(df){
  list(
    "r" = sd(df$lN),
    "lK" = log(mean(exp(df$lN))),
    "log_sigma" = sd(df$lN)
  )
}


ricker_sim <- function(r, K, N0, sigma, t = 100, as_series = FALSE, seed = NULL){
  out <- numeric(t)
  out[1] <- N0 
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  for(i in seq_len(t - 1)){
    out[i + 1] <- out[i] * exp(r * (1 - out[i] / K) + rnorm(1, 0, sigma)) 
  }
  
  if(as_series){
    out <- series_make(
      seq_along(out),
      out, 
      "ricker_sim"
    )
  }
  return(out)
}

