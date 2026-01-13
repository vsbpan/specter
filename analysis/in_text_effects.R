source("get_data/data_prep.R")

library(brms)

list.files("rds_files", full.names = TRUE) %>% 
  lapply(function(x){
    nm <- tools::file_path_sans_ext(basename(x))
    assign(
      nm, 
      readRDS(x),
      envir = globalenv()
    )
  }) %>% invisible()


# Overall effect for pop frequency
hypothesis_draws(GPDD_m1, hypothesis = "x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
  summarise_vec()

# R2 of time within time series (GPDD)
d_GPDD %>%
  group_by(ID) %>%
  mutate(
    y = (pop_freq / mean(pop_freq) - 1) * 100
  ) %>% 
  ungroup() %>% 
  transmute(
    x = x_median_offset_scale,
    y = y
  ) %>% 
  with(
    compute_r2(y = y, x = x, 
               beta = sample(hypothesis_draws(GPDD_m1,hypothesis = "x_median_offset_scale = 0")$H1, 1000), 
               trans = function(z) (exp(z) - 1) * 100)
  ) %>% 
  summarise_vec()

# Long GPDD time series standardized effect size
hypothesis_draws(GPDD_m4, hypothesis = "x_median_offset_scale = 0")$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * mean(d_GPDD[d_GPDD$x_length >= 20, ]$whole_x_length)} %>% 
  {. / 0.3} %>% 
  summarise_vec()

# Overall effect for masting frequency
hypothesis_draws(mast_m1, hypothesis = "x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_mast$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
  summarise_vec()

# R2 of time within time series (masting)
d_mast %>%
  group_by(ID) %>%
  mutate(
    y = (pop_freq / mean(pop_freq) - 1) * 100
  ) %>% 
  ungroup() %>% 
  transmute(
    x = x_median_offset_scale,
    y = y
  ) %>% 
  with(
    compute_r2(y = y, x = x, 
               beta = sample(hypothesis_draws(mast_m1,hypothesis = "x_median_offset_scale = 0")$H1, 1000), 
               trans = function(z) (exp(z) - 1) * 100)
  ) %>% 
  summarise_vec()


# Long MASTTREE+ time series standardized effect size
hypothesis_draws(mast_m4, hypothesis = "x_median_offset_scale = 0")$H1 %>% 
  {. / sd(d_mast$x_median_offset) * mean(d_mast[d_mast$x_length >= 20, ]$whole_x_length)} %>% 
  {. / 0.3} %>% 
  summarise_vec()


# Change in pop growth rate for GPDD  
hypothesis_draws(GPDD_m5, hypothesis = "estimateroffsetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * 10} %>% 
  summarise_vec()

# Change in temperature forcing frequency for GPDD 
hypothesis_draws(GPDD_m5, hypothesis = "templogfreqoffsetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
  summarise_vec()

# Change in distance to saddle for GPDD 
hypothesis_draws(GPDD_m5, hypothesis = "yminlogoffesetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
  summarise_vec()

# Change in inverse temperature for GPDD 
hypothesis_draws(GPDD_m5, hypothesis = "invtempoffsetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_GPDD$x_median_offset) * 10} %>% 
  summarise_vec()

# Change in inverse temperature for MASTTREE+
hypothesis_draws(mast_m5, hypothesis = "invtempoffsetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_mast$x_median_offset) * 10} %>% 
  summarise_vec()

# Change in temperature forcing frequency for MASTTREE+ 
hypothesis_draws(mast_m5, hypothesis = "templogfreqoffsetscale_x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_mast$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
  summarise_vec()

