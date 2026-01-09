source("get_data/data_prep.R")
library(brms)


get_prior(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    inv_temp_offset_scale +
    inv_temp_mean_scale + 
    temp_log_freq_offset_scale + 
    temp_log_freq_mean_scale + 
    (1|site_number) + 
    (1|variable/unit) + 
    (1|study_id) + 
    (1|species) +
    (1|tree_name) + 
    (1|ID), 
  data = d_mast,
  family = Gamma(link = "log"),
  data2 = list(tree_name = tre_mast$vcv)
)

prior2 <- c(
  set_prior("normal(0, 0.1)", class = "sd"),
  set_prior("normal(0, 0.05)", class = "b", coef = "x_median_offset_scale"),
  set_prior("normal(0, 0.1)", class = "b"),
  set_prior("normal(-1.2, 0.5)", class = "Intercept"),
  set_prior("gamma(3, 0.05)", class = "shape")
)

# Full dataset
mast_m1 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|site_number) + 
    (1|variable/unit) + 
    (1|study_id) + 
    (1|species) +
    (1|tree_name) + 
    (1|ID), 
  data = d_mast,
  family = Gamma(link = "log"),
  data2 = list(tree_name = tre_mast$vcv),
  prior = prior2,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)


# Only periodic
mast_m2 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|site_number) + 
    (1|variable/unit) + 
    (1|study_id) + 
    (1|species) +
    (1|tree_name) + 
    (1|ID), 
  data = d_mast %>% filter(periodic),
  family = Gamma(link = "log"),
  data2 = list(tree_name = tre_mast$vcv),
  prior = prior2,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)

# No missing data
mast_m3 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|site_number) + 
    (1|variable/unit) + 
    (1|study_id) + 
    (1|species) +
    (1|tree_name) + 
    (1|ID), 
  data = d_mast %>% filter(p_missing == 0),
  family = Gamma(link = "log"),
  data2 = list(tree_name = tre_mast$vcv),
  prior = prior2,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)


# Long time series (>= 20 years)
mast_m4 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|site_number) + 
    (1|variable/unit) + 
    (1|study_id) + 
    (1|species) +
    (1|tree_name) + 
    (1|ID), 
  data = d_mast %>% filter(x_length >= 20),
  family = Gamma(link = "log"),
  data2 = list(tree_name = tre_mast$vcv),
  prior = prior2,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.95),
  thin = 4
)

saveRDS(mast_m1, "rds_files/mast_m1.rds")
saveRDS(mast_m2, "rds_files/mast_m2.rds")
saveRDS(mast_m3, "rds_files/mast_m3.rds")
saveRDS(mast_m4, "rds_files/mast_m4.rds")




prior2.1 <- c(
  # Propfreq
  set_prior("normal(0, 0.1)", class = "sd", resp = "popfreq"),
  set_prior("normal(0, 0.01)", class = "sd", resp = "popfreq", group = "tree_name"),
  set_prior("normal(0, 0.01)", class = "sd", resp = "popfreq", group = "species"),
  set_prior("normal(0, 0.05)", class = "b", coef = "x_median_offset_scale", resp = "popfreq"),
  set_prior("normal(0, 0.1)", class = "b", resp = "popfreq"),
  set_prior("normal(-1.2, 0.5)", class = "Intercept", resp = "popfreq"),
  set_prior("gamma(3, 0.05)", class = "shape", resp = "popfreq"),
  # templogfreqoffsetscale
  set_prior("normal(0, 0.1)", class = "sd", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "templogfreqoffsetscale"),
  # invtempoffsetscale
  set_prior("normal(0, 0.1)", class = "sd", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "invtempoffsetscale")
)


# Add moderators
mast_m5 <- brm(
  bf(
    pop_freq ~ 
      x_median_offset_scale +
      x_median_mean_scale +
      x_length_log_scale + 
      inv_temp_offset_scale +
      inv_temp_mean_scale + 
      temp_log_freq_offset_scale + 
      temp_log_freq_mean_scale + 
      (1|site_number) + 
      (1|variable/unit) + 
      (1|study_id) + 
      (1|species) +
      (1|tree_name) + 
      (1|ID),
    family = Gamma(link = "log")
  ) + 
    bf(
      inv_temp_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|site_number) + 
        (1|variable/unit) + 
        (1|study_id) + 
        (1|species) +
        (1|tree_name) + 
        (1|ID),
      family = gaussian()
    ) + 
    bf(
      temp_log_freq_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|site_number) + 
        (1|variable/unit) + 
        (1|study_id) + 
        (1|species) +
        (1|tree_name) + 
        (1|ID),
      family = gaussian()
    ), 
  data = d_mast2,
  data2 = list(tree_name = tre_mast$vcv),
  prior = prior2.1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)
saveRDS(mast_m5, "rds_files/mast_m5.rds")
