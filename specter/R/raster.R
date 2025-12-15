extract2 <- function(rast, lon, lat, year){
  stopifnot(is.SpatRaster(rast))
  yr <- lubridate::year(terra::time(rast))
  
  
  yr_setdiff <- base::setdiff(year, yr)
  if(length(yr_setdiff) > 0){
    cli::cli_warn("Returning NAs for years not found in the raster ({min(yr)} - {max(yr)}): {.val {yr_setdiff}}")
  }
  
  ind <- match(year, yr)
  ny <- length(ind)
  
  res <- terra::extract(rast[[na.omit(ind)]], 
                        cbind(lon, lat)
  )
  out <- matrix(
    rep(NA_real_, length(lon) * ny), ncol = ny
  )
  out[, !is.na(ind)] <- as.matrix(res)
  out
  
}