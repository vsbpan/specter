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


# Overall effect for masting frequency
hypothesis_draws(mast_m1, hypothesis = "x_median_offset_scale = 0") %>% 
  .$H1 %>% 
  {. / sd(d_mast$x_median_offset) * 10} %>% 
  exp() %>% 
  {(. - 1) * 100} %>% 
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

