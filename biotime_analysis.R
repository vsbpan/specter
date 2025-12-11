library(vmisc)
source("indices.R")
source("series_utils.R")
# d <- readRDS("biotime_v2_full_2025/biotime_v2_full_2025.rds")
# 
# d <- d %>%
#   rename_all(tolower) %>%
#   group_by(
#     newid, latitude, longitude, depth, study_id
#   ) %>%
#   mutate(
#     series_id = paste0("series_", cur_group_id())
#   ) %>%
#   ungroup() %>%
#   mutate_at(vars(latitude, longitude, depth, newid, study_id, series_id), as.character)
# 
# d <- d %>%
#   filter(resolution == "species") %>%
#   assign_chunk(n_chunks = 200, group = "series_id") %>%
#   group_by(chunk_id) %>%
#   group_split() %>%
#   vmisc::pb_par_lapply(
#     function(x){
#       x %>%
#         group_by(series_id, year) %>%
#         summarise(
#           latitude = latitude[1],
#           longitude = longitude[1],
#           study_id = study_id[1],
#           depth = depth[1],
#           id_species = id_species[1],
#           newid = newid[1],
#           taxon = taxon[1],
#           biomass = max(biomass, na.rm = TRUE),
#           abundance = max(abundance, na.rm = TRUE)
#         )
#     }, cores = 8, inorder = FALSE
#   ) %>%
#   do.call("bind_rows", .)
#  
# d <- d %>%
#   filter(
#     n() > 7
#   ) %>%
#   arrange(series_id, year)
# saveRDS(d, "Biotime_grouped.rds")

# d <- readRDS("Biotime_grouped.rds")
# 
# d <- d %>%
#   ungroup() %>%
#   tidyr::gather(value = val, key = type, biomass:abundance) %>%
#   mutate(
#     series_id2 = paste0(series_id,"_", type)
#   ) %>%
#   filter(
#     is.finite(val)
#   ) %>%
#   group_by(
#     series_id2
#   ) %>%
#   filter(
#     n() > 7
#   )
# 
# 
# saveRDS(d, "Biotime_grouped2.rds")
d <- readRDS("Biotime_grouped2.rds")


# d %>%
#   summarise(
#     obj = list(
#       make_series(x = year, y = val, ID = series_id2)
#     )
#   ) %>%
#   assign_chunk(n_chunks = 240) %>%
#   group_by(chunk_id) %>%
#   summarise(
#     chunk = list(
#       obj
#     )
#   ) %>%
#   .$chunk %>%
#   vmisc::pb_par_lapply(
#     function(x){
#       source("indices.R")
#       source("series_utils.R")
#       lapply(x, pipeline_wrapper) %>%
#         do.call("rbind", .)
#     }, cores = 8, inorder = FALSE
#   ) %>%
#   do.call("rbind", .) -> d_res
# 
# saveRDS(d_res, "Biotime_results.rds")


d_res <- readRDS("Biotime_results.rds")
d_meta <- read_csv("biotime_v2_full_2025/biotime_v2_metadata_2025.csv") %>% 
  rename_all(tolower) %>% 
  mutate(
    study_id = as.character(study_id)
  )

d_cleaned <- d_res %>% 
  left_join(
    d %>% 
      ungroup() %>% 
      select(-c(year, val,id_species)) %>% 
      distinct() %>% 
      mutate(
        ID = series_id2
      )
  ) %>% 
  as_tibble() %>% 
  filter(!error & !is.na(mean_freq)) %>% 
  group_by(ID) %>% 
  filter(
    n() == 2
  ) %>% 
  ungroup() %>% 
  filter(type == "abundance") %>% 
  left_join(
    d_meta, by = "study_id"
  ) %>% 
  filter(
    abundance_type != "Presence/Absence" & 
      is.na(treatment) & 
      taxa != "Multiple"
  )


library(glmmTMB)


m <- glmmTMB(
  mean_freq ~ 
    x_median_offset + 
    scale(x_median_mean) +
    scale(x_length_offset) + 
    scale(x_length_mean) +  
    scale(p_nm_mean) + 
    scale(p_nm_offset) + 
    #(1|study_id) + 
    (1|taxa/newid) + 
    (1|ID), 
  data = d_cleaned %>% 
    mutate(
      p_nm = 1 - y_missing / y_n
    ) %>% 
    group_by(ID) %>% 
    #filter(all(n_freq > 0)) %>%
    mutate(
      p_nm_mean = mean(log(p_nm)),
      p_nm_offset = log(p_nm) - p_nm_mean,
      x_length_mean = mean(log(x_length)),
      x_length_offset = log(x_length) - x_length_mean,
      x_median_mean = mean(x_median),
      x_median_offset = x_median - x_median_mean
    ) %>% 
    filter(
      n() == 2
    ) %>% 
    ungroup() %>% 
    filter(whole_x_length > 30) %>% 
    filter(
      series_id %in% sample(series_id, pmin(10000,length(series_id)), replace = FALSE)
    ), 
  family = Gamma(link = "log")
); summary(m)
