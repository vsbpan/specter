find_series_vars <- function(y, index = c("mean", "var", "cv", "log_mean", "inv_mean"), name_append = "y_"){
  y <- y[!is.na(y)]
  
  if(length(y) < 1){
    out <- as.list(rep(NA_real_, length(index)))
  } else {
    out <- lapply(index, function(f){
      f <- match.fun(f)
      tryCatch({
        f(y)
      }, error = function(e){
        cli::cli_warn(
          c(e$message, "Returning NAs")
        )
        NA_real_
      })
    })
  }
  
  setNames(out, paste0(name_append, index))
}

find_years_vars <- function(x, index = c("min", "max", "median", "length"), name_append = "x_"){
  x <- x[!is.na(x)]
  if(length(x) < 1){
    out <- as.list(rep(NA_real_, length(index)))
  } else {
    x <- seq(min(x), max(x), by = 1)
    out <- lapply(index, function(f){
      f <- match.fun(f)
      tryCatch({
        f(x)
      }, error = function(e){
        cli::cli_warn(
          c(e$message, "Returning NAs")
        )
        NA_real_
      })
    })
    
  }
  
  setNames(out, paste0(name_append, index))
}


find_series_sampling <- function(y, name_append = "y_"){
  n <- length(y)
  nms <- sum(!is.na(y))
  res <- list(
    "n" = n,
    "not_missing" = nms,
    "missing" = n - nms 
  )
  names(res) <- paste0(name_append, names(res))
  res
}

find_spectrum_indices <- function(freq, power, y, index = c("mean_freq", "n_freq", "spec_exponent", 
                                                         "freq_diversity", "freq_richness"),
                                  name_append = NULL){
  if(all(is.na(power)) || all(is.na(freq)) || length(power) < 3){
    out <- as.list(rep(NA_real_, length(index)))
  } else {
    out <- lapply(index, function(f){
      f <- match.fun(f)
      tryCatch({
        f(freq, power, y)
      }, error = function(e){
        cli::cli_warn(
          c(e$message, "Returning NAs")
        )
        return(NA_real_)
      })
    })
  }
  
  if(!is.null(name_append)){
    index <- paste0(name_append, index)
  }
  setNames(out, index)
}

find_splitted_attributes <- function(series, 
                                     spec_method = c("lomb", "ndft"), 
                                     trans = c("log", "inverse"), 
                                     split_method = c("half", "equal_segment"), 
                                     len = NULL){
  res <- series %>% 
    series_calc(spec_method = spec_method, 
                name_append = "whole_", 
                trans = trans,
                drop_calc = TRUE, 
                restore = TRUE) %>% 
    series_split(method = split_method, len = len) %>% 
    lapply(function(x){
      series_calc(x, spec_method = spec_method, trans = trans) %>% 
        collect_attributes()
    }) %>% 
    do.call("rbind", .)
  rownames(res) <- NULL
  res
}
