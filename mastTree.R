library(tidyverse)
vmisc::load_all2("specter")


d <- read_csv("raw_data/MASTREEplus_2024-06-26_V2.csv") %>% 
  rename_all(tolower) %>% 
  mutate(
    series_id = paste0("series", study_id,"_site",site_number,"_species",species_code,"_varialbe_",variable)
  ) %>% 
  filter(
    vartype == "C"
  ) %>% 
  filter(
    ! unit %in% c("index")
  ) %>% 
  filter(
    species != "Mixed species"
  )


d %>% 
  group_by(series_id, year) %>% 
  summarise(
    value = max(value)
  ) %>% 
  summarise(
    obj = list(
      series_make(x = year, y = value, ID = series_id)
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
      lapply(x, find_splitted_attributes, spec_method = "lomb") %>% 
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
    d %>% 
      mutate(
        ID = series_id
      ) %>% 
      select(
        ID, spatial_unit, mono_poly, variable, site_number, study_id, latitude, variable,species,unit
      ) %>% 
      unique()
  ) %>% 
  mutate(
    part = paste0("part", part)
  )


library(glmmTMB)

m <- glmmTMB(
  mean_freq ~ 
    x_median_offset + 
    x_median_offset:scale(x_length) + 
    scale(x_median_mean) +
    scale(x_length_offset) +
    scale(x_length_mean) +
    scale(p_nm_mean) +
    scale(p_nm_offset) +
    (1|spatial_unit) + 
    (1|variable) + 
    (1|study_id) + 
    (1|species) + 
    (1|ID), 
  data = d_cleaned %>% 
    group_by(ID) %>% 
    mutate(
      p_nm = 1 - y_missing / y_n,
      p_nm_mean = mean(log(p_nm)),
      p_nm_offset = log(p_nm) - p_nm_mean,
      x_length_mean = mean(log(x_length)),
      x_length_offset = log(x_length) - x_length_mean,
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




d_cleaned <- d_cleaned %>% 
  mutate(
    genus = gsub(" .*", "", species)
  ) %>% 
  mutate(
    family = match_family(genus)
  )

d_cleaned %>% 
  group_by(ID) %>% 
  mutate(
    p_nm = 1 - y_missing / y_n,
    p_nm_mean = mean(log(p_nm)),
    p_nm_offset = log(p_nm) - p_nm_mean,
    x_length_mean = mean(log(x_length)),
    x_length_offset = log(x_length) - x_length_mean,
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
  arrange(part) %>% 
  group_by(ID) %>% 
  summarise(
    diff = diff(mean_freq),
    N = mean(x_length)
  ) %>% 
  ggplot(aes(x = N, y = diff)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm")


  geom_histogram(bins = 1000)





d$unit %>% unique()


