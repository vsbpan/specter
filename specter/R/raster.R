extract2 <- function(rast, lon, lat, year){
  stopifnot(length(lat) == length(lon))
  if(length(year) == 0 || length(lat) == 0){
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  stopifnot(is.SpatRaster(rast))
  yr <- lubridate::year(terra::time(rast))
  
  
  yr_setdiff <- base::setdiff(year, yr)
  if(length(yr_setdiff) > 0){
    cli::cli_warn("Returning NAs for years not found in the raster ({min(yr)} - {max(yr)}): {.val {yr_setdiff}}")
  }
  
  ind <- match(year, yr)
  ny <- length(ind)
  ind2 <- na.omit(ind)
  if(length(ind2) > 0){
    res <- terra::extract(rast[[ind2]], 
                          cbind(lon, lat)
                          )
  } else {
    res <- numeric(0)
  }
  
  out <- matrix(
    rep(NA_real_, length(lon) * ny), ncol = ny
  )
  out[, !is.na(ind)] <- as.matrix(res)
  out
  
}

find_temperature <- function(lon, lat, year, group_id = NULL, rast_source = c("reanalysis", "GHCN")){
  rast_source <- match.arg(rast_source, several.ok = TRUE)
  stopifnot(length(year) == length(lat))
  stopifnot(length(lat) == length(lon))
  
  out <- rep(NA_real_, length(year))
  
  
  if(length(rast_source) == 2){
    if(is.null(group_id)){
      group_id <- paste(lon, lat, sep = "_")
    }
    stopifnot(length(group_id) == length(year))
    rast_id <- rep(NA_real_, length(year))
    for(g in group_id){
      if(any(
        year[g == group_id] > 2015
      )) {
        v <- 2
      } else {
        v <- 1
      }
      rast_id[g == group_id] <- v
    }
    
  } else {
    if("reanalysis" %in% rast_source){
      rast_id <- rep(1, length(year))
    }
    if("GHCN" %in% rast_source){
      rast_id <- rep(2, length(year))
    }
  }
  
  if("reanalysis" %in% rast_source){
    rast1 <- terra::rast("specter/data/annual_air_temperature_20th_century_reanalysis.nc")
    for(y in unique(year)){
      ind <- year == y & rast_id == 1
      out[ind] <- as.vector(extract2(rast1, lon[ind], lat[ind], y))
    }
  }
  if("GHCN" %in% rast_source){
    rast2 <- terra::rast("specter/data/annual_air_temperature_GHCN_CAMS.nc")
    for(y in unique(year)){
      ind <- year == y & rast_id == 2
      out[ind] <- as.vector(extract2(rast2, lon[ind], lat[ind], y))
    }
  }
  
  
  out
}



