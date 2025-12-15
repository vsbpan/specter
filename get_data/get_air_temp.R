library(tidyverse)
vmisc::load_all2("specter")
library(terra)

# NOAA/CIRES/DOE 20th Century Reanalysis (V3)
# https://psl.noaa.gov/data/gridded/data.20thC_ReanV3.html
# /Datasets/20thC_ReanV3/Monthlies/2mSI-MO/air.2m.mon.mean.nc
rast <- terra::rast("invisible/raw_rast/air.2m.mon.mean.nc")

yr <- lubridate::year(terra::time(rast))

# compute geometric mean at each cell for each year
foo <- terra::tapp(rast, index = yr, 
                    fun = function(x){
                      exp(mean(log(x)))
                    })



names(foo) <- paste0("air_", unique(yr))
terra::time(foo) <- as.POSIXct(paste0(unique(yr), "-01-01"), tz = "UTC")
terra::units(foo) <- rep(unique(terra::units(rast)), terra::nlyr(foo))
terra::varnames(foo) <- "air"
terra::longnames(foo) <- "Annual Air Temperature at 2 m"
terra::ext(foo) <- terra::ext(foo) + c(-0.5, 0.5, 0, 0)
foo <- terra::rotate(foo)

foo %>% 
  terra::writeCDF(., "cleaned_data/annual_air_temperature_20th_century_reanalysis.nc", 
                  varname = terra::varnames(.), 
                  unit = terra::units(.),
                  longname = terra::longnames(.),
                  overwrite = TRUE, 
                  compression = 9)


# GHCN_CAMS Gridded 2m Temperature (Land)
# https://psl.noaa.gov/data/gridded/data.ghcncams.html
# /Datasets/ghcncams/air.mon.mean.nc
rast <- terra::rast("invisible/raw_rast/air.mon.mean.nc")
yr <- lubridate::year(terra::time(rast))
foo <- terra::tapp(rast, index = yr, 
                   fun = function(x){
                     exp(mean(log(x)))
                   })


names(foo) <- paste0("air_", unique(yr))
terra::time(foo) <- as.POSIXct(paste0(unique(yr), "-01-01"), tz = "UTC")
terra::units(foo) <- rep(unique(terra::units(rast)), terra::nlyr(foo))
terra::varnames(foo) <- "air"
terra::longnames(foo) <- "Annual Air Temperature at 2 m"
foo <- terra::subset(foo, -terra::nlyr(foo)) # Drop 2025 bc data is incomplete
foo <- terra::rotate(foo)

foo %>% 
  terra::writeCDF(., "cleaned_data/annual_air_temperature_GHCN_CAMS.nc", 
                varname = terra::varnames(.), 
                unit = terra::units(.),
                longname = terra::longnames(.),
                overwrite = TRUE, 
                compression = 9)



