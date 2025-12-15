library(tidyverse)
vmisc::load_all2("specter")
library(terra)

# https://psl.noaa.gov/data/gridded/data.20thC_ReanV3.html
rast <- terra::rast("invisible/air.2m.mon.mean.nc")

yr <- lubridate::year(time(rast))

# compute geometric mean at each cell for each year
foo <- terra::tapp(rast, index = yr, 
                    fun = function(x){
                      exp(mean(log(x)))
                    })



terra::names(foo) <- paste0("air_", unique(yr))
terra::time(foo) <- as.POSIXct(paste0(unique(yr), "-01-01"), tz = "UTC")
terra::units(foo) <- rep(unique(terra::units(rast)), terra::nlyr(foo))
terra::varnames(foo) <- "air (Annual Air Temperature at 2 m)"


terra::writeCDF(foo, "cleaned_data/annual_air_temperature.nc")





