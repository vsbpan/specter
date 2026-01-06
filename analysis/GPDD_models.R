source("get_data/data_prep.R")




m <- glmmTMB(
  pop_freq ~ 
    x_median_offset +
    scale(x_median_mean) +
    (1|datasourceid) + 
    (1|class/name_cleaned) + 
    (1|ID), 
  data = d_GPDD %>% 
    filter(p_missing == 0),
  family = Gamma(link = "log")
); summary(m)
