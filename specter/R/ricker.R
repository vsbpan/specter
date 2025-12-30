series_format <- function(series, keep_year = FALSE){
  series <- series_gapfill(series) %>% 
    series_transform()
  epsilon <- series$attributes$epsilon
  if(is.null(epsilon)){
    epsilon <- 0
  }
  if(any(series$data$y < 0)){
    lN <- series$data$y
  } else {
    lN <- ifelse(
      series$data$y == 0,
      series$data$y + epsilon,
      series$data$y
    )
    lN <- log(lN) 
  }
  
  if(any(!is.finite(lN) & !is.na(lN))){
    cli::cli_warn("Detected {sum(!is.finite(lN))} non-finite value{?s} in series after transforming.")
  }
  
  out <- data.frame(
    "lN" = lN,
    "lN_lag" = dplyr::lag(lN, n = 1)
  )
  
  if(keep_year){
    out <- cbind(out, "year" = series$data$x)
  }
  out <- out[is.finite(out$lN) & is.finite(out$lN_lag),]
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



ricker_fit <- function(series, prior = NULL, update_params = TRUE, opt_method = "auto", ...){
  df <- series_format(series, keep_year = TRUE)
  ricker_params <- c("r", "lK", "log_sigma")
  
  if(!is.null(prior)){
    pn <- names(prior)
    
    if(!identical(sort(ricker_params), sort(pn))){
      cli::cli_abort("The provided prior must be {.val NULL} or be a unique list of {.cls distr} objects for each of the parameters {.val {ricker_params}}.")
    }
  }
  
  opt_method_og <- opt_method
  if(opt_method == "auto"){
    opt_method <- "Nelder-Mead"
  }
  
  mod_start <- ricker_auto_start(df)
  fit_mod <- function(start, method, ...){
    out <- mle_fit(
      fn = formula(
        lN ~ lN_lag + r * (1 - exp(lN_lag - lK))
      ), 
      data = df, 
      start = start, 
      prior = prior, 
      dist = "gaussian",
      method = method,
      ...)
    out
  }
  
  m <- tryCatch({
    fit_mod(mod_start, method = opt_method, ...)
  }, error = function(e){
    cli::cli_alert_danger(sprintf("Error while fitting model:\n%s", crayon::yellow(e$message)))
    return(e)
  })
  
  if((is.error(m) || !converged(m)) && opt_method_og == "auto"){
    for (optim_method in c("BFGS", "LBFGSB3","nlminb","CG","SANN")){
      m <- tryCatch({
        fit_mod(mod_start, method = optim_method, ...)
      }, error = function(e){
        cli::cli_alert_danger(sprintf("Error while fitting model:\n%s", crayon::yellow(e$message)))
        return(e)
      })
      if(!is.error(m) && converged(m)){
        break
      }
    }
  }
  
  if(isTRUE(update_params)){
    m <- update_params(m, 
                         old_param = ricker_params, 
                         new_param = c("r", "K", "sigma"),
                         trans = list(
                           function(x) x,
                           function(x) exp(x),
                           function(x) exp(x)
                         )) 
  }
  class(m) <- c("ricker_fit", class(m))
  m
}

predict.ricker_fit <- function(object, ci = FALSE, ...){
  if(!is.null(object$vcov_og)){
    object$vcov <- object$vcov_og 
    object$par <- object$par_og 
  }
  NextMethod("predict")
}


plot.ricker_fit <- function(x, xlab = "year", ylab = "value", log = "y", ...){
  pred <- predict(x, ci = 0.95, newdata = x$data)
  dat <- cbind(pred, x$data)
  
  with(dat, {
    plot(exp(lN) ~ year, col = "black", type = "l", xlab = xlab, ylab = ylab, log = log, ...)
    lines(
      exp(estimate) ~ year, col = "red"
    )
    lines(
      exp(lower) ~ year, col = "red", lty = 2
    )
    lines(
      exp(upper) ~ year, col = "red", lty = 2
    )
  })
}

coef_frame <- function(x, ...){
  stopifnot(is.mle_fit(x))
  x %>% 
    summary(...) %>% 
    as.matrix() %>% 
    vmisc::flatten_mat_name() %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(
      "converged" = converged(x)
    )
}

find_ricker_coef <- function(series, prior = NULL, name_append = NULL){
  res <- ricker_fit(series, 
             prior = prior) %>% 
    coef_frame()
  names(res) <- gsub("__", ".", names(res))
  names(res) <- paste0(name_append, names(res))
  res
}

find_splitted_ricker_coef <- function(series, prior = NULL, split_method = c("half", "equal_segment"), len = NULL){
  res <- series %>% 
    bind_attributes(
      .,
      find_ricker_coef(., prior = prior, name_append = "whole_")
    ) %>%  
    series_split(method = split_method, len = len) %>% 
    lapply(function(x){
      bind_attributes(
        x,
        find_ricker_coef(x, prior = prior)
      ) %>% 
        collect_attributes()
    }) %>% 
    do.call("rbind", .)
  rownames(res) <- NULL
  res
}


