plot.series <- function(series, type = "l", xlab = "year", ylab = "value", log = "", ...){
  if(!("data" %in% names(series))){
    series <- series_gapfill(series)
  }
  with(
    series,
    {
      plot(data$y ~ data$x, type = type, xlab = xlab, ylab = ylab, log = log, ...)
    }
  )
}