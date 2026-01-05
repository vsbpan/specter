library(tidyverse)
vmisc::load_all2("specter")


d <- read_csv("cleaned_data/mastTree_series_cleaned.csv")
d_meta <- read_csv("cleaned_data/mastTree_meta_cleaned.csv")


d %>% 
  group_by(series_id) %>% 
  summarise(
    obj = list(
      list(
        "value" = series_make(x = year, y = value, ID = series_id),
        "temperature" = series_make(x = year, y = air_temp, ID = series_id)
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
        find_splitted_attributes(x$value, trans = "log")
      }) %>% 
        do.call("rbind", .)
      res2 <- lapply(x, function(x){
        find_splitted_attributes(x$temperature, trans = "inverse")
      }) %>% 
        do.call("rbind", .)

      list(
        "value" = res1,
        "temperature" = res2
      )
    }, cores = 8, inorder = FALSE
  ) -> d_res

d_res_pop <- map(d_res, "value") %>% do.call("rbind", .)
d_res_temp <- map(d_res, "temperature") %>% do.call("rbind", .)

write_csv(d_res_pop, "cleaned_data/mastTree_pop_res.csv")
write_csv(d_res_temp, "cleaned_data/mastTree_temp_res.csv")


