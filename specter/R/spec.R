power_spec <- function(series){
  n <- length(series$x)
  n2 <- floor(n/2)
  if(has_error(series) || n2 < 1){
    series$freq <- rep(NA_real_, n2)
    series$power <- rep(NA_real_, n2)
  } else {
    freq <- seq(1, n2, by = 1)/n
    pow <- (Mod(
      ndft(
        f = series$y, nu = series$x - min(series$x)
      )
    )^2)[2:(n2 + 1)]
    series$freq <- freq
    series$power <- pow / n2
  }
  series
}

# Modified from cooltools::ndft
ndft <- function (f, nu = seq(0, length(f) - 1), 
                  inverse = FALSE){
  N <- length(f)
  x <- seq(0, N - 1)/N
  
  if (N <= 1)
    stop("f must be a vector with more than one elements")
  
  k <- 2 * pi * nu
  
  if (length(x) != N)
    stop("x and f must be vectors of the same length")
  
  g <- colSums(f * exp(-(0+1i) * cbind(x) %*% rbind(k)))
  return(g)
}

# cooltools::ndft source code
# ndft <- function (f, x = seq(0, length(f) - 1)/length(f), nu = seq(0, 
#                                                            length(f) - 1), inverse = FALSE, weighing = TRUE, simplify = TRUE) 
# {
#   N = length(f)
#   if (N <= 1) 
#     stop("f must be a vector with more than one elements")
#   k = 2 * pi * nu
#   if (inverse) {
#     if (length(nu) != N) 
#       stop("nu and f must be vectors of the same length")
#     if (weighing) {
#       w = c(x[2] - x[1], (x[3:N] - x[1:(N - 2)])/2, x[N] - 
#               x[N - 1])
#       w = w/sum(w) * N
#     }
#     else {
#       w = 1
#     }
#     g = colSums(w * f * exp(+(0+1i) * cbind(k) %*% rbind(x)))/length(f)
#   }
#   else {
#     if (length(x) != N) 
#       stop("x and f must be vectors of the same length")
#     if (weighing) {
#       w = c(x[2] - x[1], (x[3:N] - x[1:(N - 2)])/2, x[N] - 
#               x[N - 1])
#       w = w/sum(w) * N
#     }
#     else {
#       w = 1
#     }
#     g = colSums(w * f * exp(-(0+1i) * cbind(x) %*% rbind(k)))
#   }
#   if (simplify) {
#     if (mean(abs(Im(g)))/(mean(abs(g)) + .Machine$double.xmin) < 
#         1e-13) 
#       g = Re(g)
#   }
#   return(g)

# For testing only
power_spec2 <- function(series){
  n <- length(series$x)
  n2 <- floor(n/2)
  freq <- seq(1, n2, by = 1)/n
  pow <- (Mod(fft(series$y))^2)[2:(n2 + 1)]
  series$freq <- freq
  series$power <- pow / n2
  series
}

