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
  assign_chunk() %>% 
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
        find_splitted_attributes(x$population)
      }) %>% 
        do.call("rbind", .)
      res2 <- lapply(x, function(x){
        find_splitted_attributes(x$temperature)
      }) %>% 
        do.call("rbind", .)
        
   list(
     "population" = res1,
     "temperature" = res2
   )
    }, cores = 8, inorder = FALSE
  ) -> d_res

d_res_pop <- map(d_res, "population") %>% do.call("rbind", .)
d_res_temp <- map(d_res, "temperature") %>% do.call("rbind", .)



d_cleaned <- d_res_pop %>% 
  filter(!error & !is.na(mean_freq)) %>% 
  group_by(ID) %>% 
  filter(
    n() == 2
  ) %>% 
  ungroup() 



d_cleaned <- d_cleaned %>% 
  left_join(
    d_meta %>% 
      mutate(
        ID = mainid
      )
  ) %>% 
  mutate(
    part = paste0("part", part)
  )




library(glmmTMB)

d_cleaned %>% 
  ggplot(aes(x = x_median, y = mean_freq)) + 
  geom_point() + 
  geom_smooth()


m <- glmmTMB(
  mean_freq ~ 
    x_median_offset +
    scale(x_median_mean) +
    #scale(x_length_offset) +
    #scale(x_length_mean) +
    #scale(p_nm_mean) +
    #scale(p_nm_offset) +
    (1|datasourceid) + 
    (1|class/name_cleaned) + 
    (1|ID), 
  data = d_cleaned %>% 
    group_by(ID) %>% 
    mutate(
      p_nm = 1 - y_missing / y_n,
      p_nm_mean = mean(log(p_nm)),
      p_nm_offset = log(p_nm) - p_nm_mean,
      x_length_mean = mean(log(x_length)),
      x_length_offset = log(x_length) - x_length_mean,
      y_cv_mean = mean(log(y_cv)),
      y_cv_offset = log(y_cv) - y_cv_mean,
      trend_mean = mean(trend),
      trend_offset = trend - trend_mean,
      x_median_mean = mean(x_median),
      x_median_offset = x_median - x_median_mean
    ) %>% 
    ungroup() %>% 
    filter(
      p_nm > 0.8 & x_length > 5
    ) %>% 
    filter(
      p_nm == 1
    ) %>%
    group_by(ID) %>% 
    filter(all(whole_n_freq > 0)) %>%
    filter(
      n() == 2
    ) %>% 
    # filter(
    #   diff(x_length) == 0
    # ) %>%
    ungroup(), 
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")),
  family = Gamma(link = "log")
); summary(m)


m <- gamm4::gamm4(
  mean_freq ~ 
    s(x_median_mean, by = x_median_offset, k = -1, bs = "tp") + 
    #s(scale(x_length_mean), by = x_median_offset, k = 5, bs = "tp") + 
    scale(x_median_mean) +  
    #scale(x_length_offset) +
    scale(x_length_mean),
  random = ~ (1|datasourceid) + 
    (1|taxonomicclass) + 
    (1|ID),
  family = Gamma(link = "log"),
  data = w %>% 
    filter(
      x_median_mean > 1900
    )
); summary(m)

m$gam %>% plot()
summary(m$gam)

d_cleaned %>% 
  filter(
    sourcedimension %in% allowed_dimensions
  ) %>% 
  group_by(ID) %>% 
  mutate(
    p_nm = 1 - y_missing / y_n,
    p_nm_mean = mean(log(p_nm)),
    p_nm_offset = log(p_nm) - p_nm_mean,
    x_length_mean = mean(log(x_length)),
    x_length_offset = log(x_length) - x_length_mean,
    y_cv_mean = mean(log(y_cv)),
    y_cv_offset = log(y_cv) - y_cv_mean,
    trend_mean = mean(trend),
    trend_offset = trend - trend_mean,
    x_median_mean = mean(x_median),
    x_median_offset = x_median - x_median_mean
  ) %>% 
  ungroup() %>% 
  filter(
    p_nm > 0.8 & x_length > 5
  ) %>% 
  # filter(
  #   p_nm == 1
  # ) %>%
  group_by(ID) %>% 
  #filter(all(n_freq > 0)) %>%
  filter(
    n() == 2
  ) %>% 
  # filter(
  #   diff(x_length) == 0
  # ) %>%
  ungroup() %>% 
  mutate_at(
    .vars = vars(ID, taxonomicclass,datasourceid), as.factor
  ) -> w



