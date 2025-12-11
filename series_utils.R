has_error <- function(obj){
  isTRUE(obj$attributes$error)
}

set_error <- function(obj){
  obj$attributes$error <- TRUE
  obj
}

bind_attributes <- function(obj,b = NULL){
  if(!is.null(b)){
    a <- obj$attributes
    if(!is.null(a)){
      obj$attributes <- c(a, b)
    } else {
      obj$attributes <- b
    }
  }
  obj
}

series_gapfill <- function(series) {
  x <- series$x
  y <- series$y
  xy_na_loc <- is.na(x) | is.na(y)
  x <- x[!xy_na_loc]
  y <- y[!xy_na_loc]
  
  time_grid <- seq(min(x), max(x), by = 1)
  
  y_filled <- rep(NA_real_, length(time_grid))
  names(y_filled) <- time_grid
  y_filled[as.character(x)] <- y 
  y_filled <- unname(y_filled)
  
  series$data <- list(
    "x" = time_grid,
    "y" = y_filled
  )
  
  series$x <- x
  series$y <- y
  
  series
}

series_transform <- function(series, trans){
  if(var(series$y, na.rm = TRUE) == 0 || length(series$y) < 3){
    series <- set_error(series)
    return(series)
  }
  if(any(series$y < 0)){
    
  } else {
    series$y <- log(series$y + min(series$y[series$y>0]) / 2)
  }
  series
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
      stats::loess(y~x, data = data.frame("x" = series$x, "y" = series$y), span = 0.5)
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
      NULL
    })
    
    if(is.null(m) || is.null(m_loess)){
      res <- NA_real_
      series <- set_error(series)
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
        series <- set_error(series)
      }
      res <- unname(stats::coef(m)[2]) 
    }
  }
  
  series <- bind_attributes(series, list(
    "trend" = res
  ))
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



find_series_vars <- function(y, index = c("mean", "var", "cv", "log_mean"), name_append = "y_"){
  y <- y[!is.na(y)]
  lapply(index, function(f){
    f <- match.fun(f)
    tryCatch({
      f(y)
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
      NA_real_
    })
  }) %>% 
    setNames(paste0(name_append, index))
}

find_years_vars <- function(x, index = c("min", "max", "median", "length"), name_append = "x_"){
  x <- x[!is.na(x)]
  x <- seq(min(x), max(x), by = 1)
  lapply(index, function(f){
    f <- match.fun(f)
    tryCatch({
      f(x)
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
      NA_real_
    })
  }) %>% 
    setNames(paste0(name_append, index))
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

find_spectrum_indices <- function(freq, power, index = c("mean_freq", "n_freq", "spec_exponent", 
                                                         "freq_diversity", "freq_richness")){
  if(all(is.na(power)) || all(is.na(freq)) || length(power) < 3){
    return(
      lapply(index, function(f){
          return(NA_real_)
      }) %>% 
        setNames(index)
    )
  }
  lapply(index, function(f){
    f <- match.fun(f)
    tryCatch({
      f(freq, power)
    }, error = function(e){
      cli::cli_warn(
        c(e$message, "Returning NAs")
      )
       return(NA_real_)
     })
  }) %>% 
    setNames(index)
}


series_calc <- function(series){
  series %>% 
    series_gapfill() %>% 
    series_transform() %>% 
    series_detrend() %>% 
    power_spec() %>% 
    bind_attributes(
      .,
      find_spectrum_indices(.$freq, .$power)
    ) %>% 
    bind_attributes(
      .,
      find_series_vars(.$data$y)
    ) %>% 
    bind_attributes(
      .,
      find_years_vars(.$data$x)
    ) %>% 
    bind_attributes(
      .,
      find_series_sampling(.$data$y)
    )
}

collect_attributes <- function(series){
  do.call("data.frame", series$attributes)
}

series_subset <- function(series, indices){
  series$x <- series$x[indices]
  series$y <- series$y[indices]
  series
}

series_split <- function(series){
  # Allow attribute inheritance 
  m <- (median(range(series$x, na.rm = TRUE)))
  
  list(
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
}

series_whole_attr <- function(series){
  series %>% 
    series_gapfill() %>% 
    bind_attributes(
      .,
      find_series_vars(.$data$y, name_append = "whole_y_")
    ) %>% 
    bind_attributes(
      .,
      find_years_vars(.$data$x, name_append = "whole_x_")
    ) %>% 
    bind_attributes(
      .,
      find_series_sampling(.$data$y, name_append = "whole_y_")
    )
}


make_series <- function(x, y, ID){
  id <- unique(ID)
  if(length(id) != 1){
    cli::cli_abort("Detected more than 1 ID.")
  }
  list(
    "x" = x, 
    "y" = y,
    "attributes" = list("ID" = id, "error" = FALSE)
  ) 
}

pipeline_wrapper <- function(series){
  res <- series %>% 
    series_whole_attr() %>% 
    series_split() %>% 
    lapply(function(x){
      series_calc(x) %>% 
        collect_attributes()
    }) %>% 
    do.call("rbind", .)
  rownames(res) <- NULL
  res
}


assign_chunk <- function(.df, n_chunks = 32, group = NULL){
  if(is.null(group)){
    n <- nrow(.df)
    s <- ceiling(n/n_chunks)
    
    .df$chunk_id <- sample(
      rep(paste0("chunk", seq_len(n_chunks)), s),
      n, 
      replace = FALSE
    ) 
  } else {
    u <- unique(.df[[group]])
    n <- length(u)
    s <- ceiling(n/n_chunks)
    
    i <- sample(
      rep(paste0("chunk", seq_len(n_chunks)), s),
      n, 
      replace = FALSE
    ) 

    .df$chunk_id <- i[match(.df[[group]], u)]
  }
  .df
}











