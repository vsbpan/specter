mean_freq <- function(freq, power, y = NULL){
  g_hat <- stats::approxfun(x = freq, y = power)
  denom <- cubature::cubintegrate(
    g_hat, lower = min(freq), upper = max(freq)
  )$integral
  # cubature::cubintegrate() is more numerically stable than stats::integrate()
  num <- cubature::cubintegrate(
    function(x){
      g_hat(x) * log(x)
    }, 
    lower = min(freq), upper = max(freq)
  )$integral
  exp(num / denom)
}

spec_exponent <- function(freq, power, y = NULL){
  if(length(freq) < 3){
    return(NA_real_)
  }
  suppressMessages(lmodel2::lmodel2(
    log(power) ~ log(freq), nperm = 0, 
  )) %>% 
    .$regression.results %>% 
    .$Slope %>% 
    .[1]
}

func_div <- function(p, y, q){
  nms <- paste0("sp", seq_along(p))
  w <- matrix(p, nrow = 1, dimnames = list("foo", nms))
  d <- matrix(c(as.matrix(dist(y))), nrow = length(p), ncol = length(p), dimnames = list(nms, nms))
  mFD::alpha.fd.hill(asb_sp_w = w, 
                     sp_dist = d, 
                     q = q)$asb_FD_Hill %>% 
    as.numeric() %>% 
    unname()
}

freq_diversity <- function(freq, power, y = NULL){
  func_div(p = power, y = freq, q = 1)
}

freq_richness <- function(freq, power, y = NULL){
  func_div(p = power, y = freq, q = 0)
}

# Press W.H., Teukolsky S.A., Vetterling S.T., Flannery, B.P. (2007) Numerical recipes in C: the art of scientific computing.3nd edition. Cambridge University Press, Cambridge, pp686.
# on Page 686: eqn 13.8.7
FAP <- function(power, sigma2 = NULL){
  if(is.null(sigma2)){
    sigma2 <- mean(power) # Approximate
  }
  
  # Take raw power and normalized by 2 * sigma^2
  z <- power / (2 * sigma2)
  M <- 2 * length(z) # Approximate M independent samples
  p <- M * exp(-z) # approximation provided by eqn 13.8.8
  
  ifelse(
    p > 0.01, 
    1 - (1 - exp(-z))^M, # More accurate
    p
  )
}


n_freq <- function(freq, power, y = NULL){
  if(is.null(y)){
    sigma2 <- NULL
  } else {
    sigma2 <- var(y, na.rm = TRUE)
  }
  sum(FAP(power, sigma2 = sigma2) < 0.05)
}


cv <- function(x, na.rm = FALSE){
  if(any(x < 0) || all(x == 0)){
    return(NA_real_)
  }
  vmisc::cv(x, na.rm = na.rm)
}



log_mean <- function(x){
  if(any(x < 0) || all(x == 0)){
    return(NA_real_)
  }
  mean(log(x + min(x[x > 0], na.rm = TRUE) / 2), na.rm = TRUE)
}
