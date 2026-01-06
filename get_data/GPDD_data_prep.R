library(tidyverse)
vmisc::load_all2("specter")

d_pop <- read_csv("cleaned_data/GPDD_pop_res.csv")
d_ricker <- read_csv("cleaned_data/GPDD_ricker_res.csv")
d_temp <- read_csv("cleaned_data/GPDD_temp_res.csv")
d_meta <- read_csv("cleaned_data/GPDD_meta_cleaned.csv")




d_temp <- d_temp %>% 
  dplyr::select(ID, part, y_mean, y_inv_mean, whole_mean_freq, mean_freq) %>% 
  rename(
    temp = y_mean,
    inv_temp = y_inv_mean,
    temp_freq = mean_freq
  )

d_ricker <- d_ricker %>% 
  filter(converged) %>% 
  dplyr::select(ID, part, starts_with("se"), starts_with("estimate"))

d_pop %>% 
  filter(!error) %>% 
  mutate(
    p_missing = y_missing / y_n,
    periodic = whole_n_freq > 0
  ) %>% 
  dplyr::select(
    ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, mean_freq, x_median, x_min, x_max
  ) %>% 
  rename(
    pop_freq = mean_freq
  ) %>% 
  filter(
    x_length > 5 & p_missing < 0.2
  ) %>% 
  group_by(ID) %>% 
  filter(
    n() == 2
  ) %>% 
  ungroup() %>% 
  filter(
   is.na(pop_freq) 
  )






d_pop <- d_pop %>% 
  filter(!error) %>% 
  mutate(
    p_missing = y_missing / y_n,
    periodic = whole_n_freq > 0
  ) %>% 
  dplyr::select(
    ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, mean_freq, x_median, x_min, x_max
  ) %>% 
  rename(
    pop_freq = mean_freq
  ) %>% 
  filter(
    x_length > 5 & p_missing < 0.2
  ) %>% 
  group_by(ID) %>% 
  filter(
    n() == 2
  ) %>% 
  ungroup()

d_pop <- d_pop %>% left_join(
  d_meta %>% 
    mutate(ID = mainid)
)

