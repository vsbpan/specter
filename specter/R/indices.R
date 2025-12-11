mean_freq <- function(freq, power){
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

spec_exponent <- function(freq, power){
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

freq_diversity <- function(freq, power){
  func_div(p = power, y = freq, q = 1)
}

freq_richness <- function(freq, power){
  func_div(p = power, y = freq, q = 0)
}

FAP <- function(power){
  exp(
    - power / mean(power)
  )
}


n_freq <- function(freq, power){
  sum(FAP(power) < 0.05)
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
