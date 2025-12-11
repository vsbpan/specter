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


series_make <- function(x, y, ID){
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
