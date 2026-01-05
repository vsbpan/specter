series_gapfill <- function(series) {
  if(has_error(series)){
    series$data <- list(
      "x" = rep(NA_real_, length(series$x)),
      "y" = rep(NA_real_, length(series$y))
    )
    return(series)
  }
  x <- series$x
  y <- series$y
  xy_na_loc <- is.na(x) | is.na(y)
  x <- x[!xy_na_loc]
  y <- y[!xy_na_loc]
  
  if(length(x) < 1){
    series <- set_error(series, reason = "series is empty")
  } else {
    time_grid <- seq(min(x), max(x), by = 1)
    y_filled <- rep(NA_real_, length(time_grid))
    names(y_filled) <- time_grid
    y_filled[as.character(x)] <- y 
    y_filled <- unname(y_filled)
    
    series$data <- list(
      "x" = time_grid,
      "y" = y_filled
    )
  }
  
  series$x <- x
  series$y <- y
  
  series
}

series_transform <- function(series, trans){
  if(has_error(series)){
    epsilon <- NA_real_
    series <- bind_attributes(series, list(
      "epsilon" = epsilon
    ))
    return(series)
  }
  if(length(series$y) < 3){
    epsilon <- NA_real_
    series <- set_error(series, reason = "length(y) < 3")
    series <- bind_attributes(series, list(
      "epsilon" = epsilon
    ))
    return(series)
  }
  if(!isTRUE(var(series$y, na.rm = TRUE) > 0)){
    epsilon <- NA_real_
    series <- set_error(series, reason = "No variance")
    series <- bind_attributes(series, list(
      "epsilon" = epsilon
    ))
    return(series)
  }

  if(any(series$y < 0)){
    epsilon <- NA_real_
  } else {
    epsilon <- min(series$y[series$y>0]) / 2
    series$y <- ifelse(series$y == 0,series$y + epsilon, series$y)
    series$y <- log(series$y)
  }
  series <- bind_attributes(series, list(
    "epsilon" = epsilon
  ))
  
  return(series)
}

series_detrend <- function(series){
  if(has_error(series) || length(series$x) < 3){
    res <- NA_real_
  } else {
    m <- tryCatch({
      stats::lm(y~x, data = data.frame("x" = series$x, "y" = series$y))
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
      NULL
    })
    m_loess <- tryCatch({
      stats::loess(y~x, data = data.frame("x" = series$x, "y" = series$y), 
                   span = 5 / length(series$x)) # Use a 5-year span
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
      NULL
    })
    
    if(is.null(m)){
      res <- NA_real_
      series <- set_error(series, reason = "lm() trend failed")
    } 
    if(is.null(m_loess)){
      series <- set_error(series, reason = "loess() fit failed")
    } else {
      # Linear detrend
      #series$y <- unname(series$y - predict(m, newdata = data.frame("x" = series$x)))
      
      # LOESS smooth
      series$y <- tryCatch(
        unname(series$y - predict(m_loess, newdata = data.frame("x" = series$x))),
        error = function(e){
          cli::cli_warn(e$message)
          rep(NA_real_, length(series$y))
        }
      )
      if(all(is.na(series$y))){
        series <- set_error(series, reason = "loess() predict failed")
      }
      res <- unname(stats::coef(m)[2]) 
    }
  }
  
  series <- bind_attributes(series, list(
    "trend" = res
  ))
  series
}



series_subset <- function(series, indices){
  series$x <- series$x[indices]
  series$y <- series$y[indices]
  series
}

series_split <- function(series, method = c("half", "equal_segment"), len = NULL){
  
  method <- match.arg(method)
  if(method == "equal_segment"){
    if(is.null(len)){
      cli::cli_abort("When splitting time series into equal segments, the {.arg len} argument must be supplied.")
    }
    if(!isTRUE(len > 0) || len %% 1 != 0){
      cli::cli_abort("When splitting time series into equal segments, the {.arg len} argument must be a positive integer.")
    }
  }
  
  
  if(method == "half"){
    # Allow attribute inheritance 
    m <- (median(range(series$x, na.rm = TRUE)))
    
    res <- list(
      "p1" = bind_attributes(
        series_subset(series, series$x <= (m)), 
        list(
          "part" = 1
        )
      ),
      "p2" = bind_attributes(
        series_subset(series, series$x >= ceiling(m)), 
        list(
          "part" = 2
        )
      )
    )
  } else {
    n <- length(series$x)
    if(n < len){
      series <- set_error(series, reason = "series length less than equal_segment len")
      res <- list(
        bind_attributes(
          series,
          list(
            "part" = 1
          )
        )
      )
    } else {
      res <- lapply(seq_len(floor(n / len)), function(k){
        bind_attributes(
          series_subset(series, seq_len(len) + (k - 1) * len),
          list(
            "part" = k
          )
        )
      }) 
    }
  }
  return(res)
}
 

series_make <- function(x, y, ID, ...){
  id <- unique(ID)
  if(length(id) != 1){
    cli::cli_abort("Detected more than 1 ID.")
  }
  if(length(x) != length(y)){
    cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
  }
  dots <- list(...)
  res <- list(
    "x" = x, 
    "y" = y,
    "attributes" = list("ID" = id, "error" = FALSE, "error_reason" = character(0))
  ) 
  out <- c(
    res,
    dots
  )
  class(out) <- c("series","list")
  out
}


series_attribute_rename <- function(series, name_append = NULL){
  new_names <- names(series$attributes)
  ind <- !new_names %in% c("ID", "error", "error_reason")
  new_names[ind] <- paste0(name_append, new_names[ind])
  names(series$attributes) <- new_names
  series
}

series_calc <- function(series, spec_method = c("lomb", "ndft"), 
                        name_append = NULL, drop_calc = FALSE, restore = FALSE){
  
  y_name <- "y_"
  x_name <- "x_"
  if(!is.null(name_append)){
    y_name <- paste0(name_append, y_name)
    x_name <- paste0(name_append, x_name)
  }
  
  
  res <- series %>% 
    series_gapfill() %>% 
    series_transform() %>% 
    series_detrend() %>% 
    power_spec(method = spec_method) %>% 
    series_attribute_rename(name_append = name_append) %>% 
    bind_attributes(
      .,
      find_spectrum_indices(.$freq, .$power, .$y, name_append = name_append)
    ) %>% 
    bind_attributes(
      .,
      find_series_vars(.$data$y, name_append = y_name)
    ) %>% 
    bind_attributes(
      .,
      find_years_vars(.$data$x, name_append = x_name)
    ) %>% 
    bind_attributes(
      .,
      find_series_sampling(.$data$y, name_append = y_name)
    )
  
  if(restore){
    res$x <- res$data$x
    res$y <- res$data$y
  }
  
  if(drop_calc){
    res$data <- NULL
    res$freq <- NULL
    res$power <- NULL
  }
  res
}


