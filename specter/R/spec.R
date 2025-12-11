power_spec <- function(series){
  n <- length(series$x)
  if(has_error(series)){
    n2 <- floor(n/2)
    series$freq <- rep(NA_real_, n2)
    series$power <- rep(NA_real_, n2)
  } else {
    freq <- seq(1, floor(n/2), by = 1)/n
    pow <- (Mod(
      cooltools::ndft(
        f = series$y, nu = series$x - min(series$x)
      )
    )^2)[2:(floor(n/2) + 1)]
    series$freq <- freq
    series$power <- pow / n 
  }
  series
}

# For testing only
# power_spec <- function(series){
#   n <- length(series$x)
#   freq <- seq(1, floor(n/2), by = 1)/n
#   pow <- (Mod(fft(series$y))^2)[2:(floor(n/2) + 1)]
#   series$freq <- freq
#   series$power <- pow / n
#   series
# }