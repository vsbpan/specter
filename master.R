library(tidyverse)
vmisc::load_all2("specter")


d <- read_csv("raw_data/df35b.233.1-DATA.csv") %>% 
  rename_all(tolower) 
d_taxa <- read_csv("raw_data/df35b.236.1-DATA.csv") %>% 
  rename_all(tolower) %>% 
  dplyr::select(-notes)
d_meta <- read_csv("raw_data/df35b.234.1-DATA.csv") %>% 
  rename_all(tolower) %>% 
  left_join(d_taxa) %>% 
  mutate(
    sourcedimension = tolower(sourcedimension)
  )

## Need to throw out morbillivirus


d %>% 
  mutate(
    series_id = paste0("series_", mainid)
  ) %>% 
  group_by(series_id, sampleyear) %>% 
  summarise(
    population = max(population)
  ) %>% 
  summarise(
    obj = list(
      series_make(x = sampleyear, y = population, ID = series_id)
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
      lapply(x, find_splitted_attributes) %>% 
        do.call("rbind", .)
    }, cores = 8, inorder = FALSE
  ) %>% 
  do.call("rbind", .) -> d_res


d_cleaned <- d_res %>% 
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
        ID = paste0("series_", mainid)
      )
  ) %>% 
  mutate(
    part = paste0("part", part)
  )




library(glmmTMB)

allowed_dimensions <- c("density", "count", "mean count", "count (estimated)", "mean concentration", "mean density")
allowed_class <- c(
  
)

m <- glmmTMB(
  mean_freq ~ 
    x_median_offset +
    scale(x_median_mean) +
    scale(x_length_offset) +
    scale(x_length_mean) +
    #scale(p_nm_mean) +
    #scale(p_nm_offset) +
    (1|datasourceid) + 
    (1|taxonomicclass/taxonomicfamily/taxonomicgenus/taxonname) + 
    (1|ID), 
  data = d_cleaned %>% 
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
    ungroup(), 
  #control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")),
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



