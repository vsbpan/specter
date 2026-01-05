library(tidyverse)
vmisc::load_all2("specter")


d <- read_csv("cleaned_data/GPDD_series_cleaned.csv")
d_meta <- read_csv("cleaned_data/GPDD_meta_cleaned.csv")

## Need to throw out morbillivirus


d %>% 
  group_by(mainid) %>% 
  summarise(
    obj = list(
      list(
        "population" = series_make(x = year, y = population, ID = mainid),
        "temperature" = series_make(x = year, y = air_temp, ID = mainid)
      )
    )
  ) %>% 
  assign_chunk(seed = 1) %>% 
  group_by(chunk_id) %>% 
  summarise(
    chunk = list(
      obj
    )
  ) %>%
  .$chunk %>% 
  vmisc::pb_par_lapply(
    function(x){
      res1 <- lapply(x, function(x){
        find_splitted_attributes(x$population, trans = "log")
      }) %>% 
        do.call("rbind", .)
      res2 <- lapply(x, function(x){
        find_splitted_attributes(x$temperature, trans = "inverse")
      }) %>% 
        do.call("rbind", .)
      
      prior <- list(
        "r" = vmisc::distr_make("norm", list("mean" = 0.5, "sd" = 1)),
        "lK" = vmisc::distr_make("norm", list("mean" = NA_real_, "sd" = 3)),
        "log_sigma" = vmisc::distr_make("norm", list("mean" = -3, "sd" = 3))
      )
      
      res3 <- lapply(x, function(x){
        prior$lK$param$mean <- log(max(x$population$y, na.rm = TRUE))
        find_splitted_ricker_coef(x$population, prior = prior)
      }) %>% 
        do.call("rbind", .)
      
      list(
        "population" = res1,
        "temperature" = res2,
        "ricker" = res3
      )
    }, cores = 8, inorder = FALSE
  ) -> d_res

d_res_pop <- map(d_res, "population") %>% do.call("rbind", .)
d_res_temp <- map(d_res, "temperature") %>% do.call("rbind", .)
d_res_ricker <- map(d_res, "ricker") %>% do.call("rbind", .)

write_csv(d_res_pop, "cleaned_data/GPDD_pop_res.csv")
write_csv(d_res_temp, "cleaned_data/GPDD_temp_res.csv")
write_csv(d_res_ricker, "cleaned_data/GPDD_ricker_res.csv")


