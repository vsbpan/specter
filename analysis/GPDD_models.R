source("get_data/data_prep.R")
library(brms)


get_prior(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    inv_temp_offset_scale +
    inv_temp_mean_scale + 
    estimate.r_offset_scale +
    estimate.r_mean_scale +
    temp_log_freq_offset_scale + 
    temp_log_freq_mean_scale + 
    y_min_log_mean_scale + 
    y_min_log_offeset_scale + 
    (1|locationid) + 
    (1|datasourceid) + 
    (1|class/family/species) +
    (1|ID), 
  data = d_GPDD,
  family = Gamma(link = "log")
)

prior1 <- c(
  set_prior("normal(0, 0.1)", class = "sd"),
  set_prior("normal(0, 0.05)", class = "b", coef = "x_median_offset_scale"),
  set_prior("normal(0, 0.1)", class = "b"),
  set_prior("normal(-1.2, 0.5)", class = "Intercept"),
  set_prior("gamma(3, 0.05)", class = "shape")
)

# Full dataset
GPDD_m1 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|locationid) + 
    (1|datasourceid) + 
    (1|class/family/species) +
    (1|ID), 
  data = d_GPDD,
  family = Gamma(link = "log"),
  prior = prior1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)


# Only periodic
GPDD_m2 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|locationid) + 
    (1|datasourceid) + 
    (1|class/family/species) +
    (1|ID), 
  data = d_GPDD %>% filter(periodic),
  family = Gamma(link = "log"),
  prior = prior1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)

# No missing data
GPDD_m3 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|locationid) + 
    (1|datasourceid) + 
    (1|class/family/species) +
    (1|ID), 
  data = d_GPDD %>% filter(p_missing == 0),
  family = Gamma(link = "log"),
  prior = prior1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)


# Long time series (>= 20 years)
GPDD_m4 <- brm(
  pop_freq ~ 
    x_median_offset_scale +
    x_median_mean_scale +
    x_length_log_scale + 
    (1|locationid) + 
    (1|datasourceid) + 
    (1|class/family/species) +
    (1|ID), 
  data = d_GPDD %>% filter(x_length >= 20),
  family = Gamma(link = "log"),
  prior = prior1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)

saveRDS(GPDD_m1, "rds_files/GPDD_m1.rds")
saveRDS(GPDD_m2, "rds_files/GPDD_m2.rds")
saveRDS(GPDD_m3, "rds_files/GPDD_m3.rds")
saveRDS(GPDD_m4, "rds_files/GPDD_m4.rds")





prior1.1 <- c(
  # Propfreq
  set_prior("normal(0, 0.1)", class = "sd", resp = "popfreq"),
  set_prior("normal(0, 0.05)", class = "b", coef = "x_median_offset_scale", resp = "popfreq"),
  set_prior("normal(0, 0.1)", class = "b", resp = "popfreq"),
  set_prior("normal(-1.2, 0.5)", class = "Intercept", resp = "popfreq"),
  set_prior("gamma(3, 0.05)", class = "shape", resp = "popfreq"),
  # templogfreqoffsetscale
  set_prior("normal(0, 1)", class = "sd", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "templogfreqoffsetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "templogfreqoffsetscale"),
  # invtempoffsetscale
  set_prior("normal(0, 1)", class = "sd", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "invtempoffsetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "invtempoffsetscale"),
  # estimateroffsetscale
  set_prior("normal(0, 1)", class = "sd", resp = "estimateroffsetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "estimateroffsetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "estimateroffsetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "estimateroffsetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "estimateroffsetscale"),
  # yminlogoffesetscale
  set_prior("normal(0, 1)", class = "sd", resp = "yminlogoffesetscale"),
  set_prior("normal(0, 1)", class = "b", coef = "x_median_offset_scale", resp = "yminlogoffesetscale"),
  set_prior("normal(0, 1)", class = "b", resp = "yminlogoffesetscale"),
  set_prior("normal(0, 1)", class = "Intercept", resp = "yminlogoffesetscale"),
  set_prior("normal(0, 1)", class = "sigma", resp = "yminlogoffesetscale")
)



get_prior(
  bf(
    pop_freq ~ 
      x_median_offset_scale +
      x_median_mean_scale +
      x_length_log_scale + 
      inv_temp_offset_scale +
      inv_temp_mean_scale + 
      estimate.r_offset_scale +
      estimate.r_mean_scale +
      temp_log_freq_offset_scale + 
      temp_log_freq_mean_scale + 
      y_min_log_mean_scale + 
      y_min_log_offeset_scale + 
      (1|locationid) + 
      (1|datasourceid) + 
      (1|class/family/species) +
      (1|ID),
    family = Gamma(link = "log")
  ) + 
    bf(
      inv_temp_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ) + 
    bf(
      temp_log_freq_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ) + 
    bf(
      estimate.r_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    )  + 
    bf(
      y_min_log_offeset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ), 
  data = d_GPDD2
)




# Add moderators
GPDD_m5 <- brm(
  bf(
    pop_freq ~ 
      x_median_offset_scale +
      x_median_mean_scale +
      x_length_log_scale + 
      inv_temp_offset_scale +
      inv_temp_mean_scale + 
      estimate.r_offset_scale +
      estimate.r_mean_scale +
      temp_log_freq_offset_scale + 
      temp_log_freq_mean_scale + 
      y_min_log_mean_scale + 
      y_min_log_offeset_scale + 
      (1|locationid) + 
      (1|datasourceid) + 
      (1|class/family/species) +
      (1|ID),
    family = Gamma(link = "log")
  ) + 
    bf(
      inv_temp_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ) + 
    bf(
      temp_log_freq_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ) + 
    bf(
      estimate.r_offset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    )  + 
    bf(
      y_min_log_offeset_scale ~ 
        x_median_offset_scale +
        x_length_log_scale + 
        (1|locationid) + 
        (1|datasourceid) + 
        (1|class/family/species) +
        (1|ID),
      family = gaussian()
    ), 
  data = d_GPDD2,
  prior = prior1.1,
  cores = 6, 
  chains = 6, 
  iter = 4000, 
  control = list(adapt_delta = 0.99),
  thin = 4
)


saveRDS(GPDD_m5, "rds_files/GPDD_m5.rds")

