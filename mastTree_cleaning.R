library(tidyverse)
vmisc::load_all2("specter")


d <- read_csv("raw_data/MASTREEplus_2024-06-26_V2.csv") %>% 
  rename_all(tolower) %>% 
  mutate(
    series_id = paste0("series", study_id,"_site",site_number,
                       "_species",species_code,"_varialbe_",variable, "_unit_", unit)
  ) %>% 
  filter(
    vartype == "C"
  ) %>% 
  filter(
    ! unit %in% c("index")
  ) %>% 
  filter(
    species != "Mixed species"
  ) %>% 
  mutate(
    lon = longitude, lat = latitude
  ) 

d_meta <- d %>% 
  dplyr::select(
    lon, lat, series_id, study_id, spatial_unit, variable, species, species_code, site_number, unit
  ) %>% 
  unique()

d <- d %>% 
  group_by(series_id, year) %>% 
  summarise(
    value = max(value, na.rm = TRUE)
  ) %>% 
  dplyr::filter(
    !is.na(year) & !is.na(value)
  ) %>% # First pass filter by length >= 8 years
  dplyr::group_by(series_id) %>% 
  dplyr::filter(
    n() >= 8
  ) %>% 
  ungroup()

d_meta <- d_meta %>% # keep only those datasets that met the first pass screening criteria
  dplyr::filter(series_id %in% unique(d$series_id))


# Add taxon info
d_taxon2 <- readr::read_csv("cleaned_data/mastTree_taxon_info.csv") %>% 
  dplyr::select(
    species, ott_id, name_cleaned
  ) %>% 
  dplyr::filter(
    species %in% unique(d_meta$species)
  ) 

d_taxon2 <- d_taxon2 %>% 
  dplyr::mutate(
    ncbi_id = ott2ncbi(ott_id),
    ncbi2taxonomy(ncbi_id),
    taxon_rank = ncbi2rank(ncbi_id)
  )

# Try using itis instead of ncbi
w <- d_taxon2 %>% 
  filter(is.na(ncbi_id)) %>% 
  mutate(
    itis_id = ott2itis(ott_id),
    itis2taxonomy(itis_id),
    taxon_rank = itis2rank(itis_id)
  ) 
d_taxon2 <- d_taxon2 %>% 
  filter(!is.na(ncbi_id)) %>% 
  rbind(
    w %>% select(-itis_id)
  ) 

d_meta <- d_meta %>%
  rename(taxonname = species) %>% 
  dplyr::left_join(
    d_taxon2,
    by = "taxonname"
  )

z <- d %>% 
  group_by(series_id) %>% 
  summarise(
    obj = list(series_make(year, value, series_id))
  ) 

z$obj <- lapply(z$obj, series_gapfill)
d <- lapply(z$obj, function(o){
  data.frame(
    "series_id" = o$attributes$ID,
    "year" = o$data$x,
    "value" = o$data$y
  )
}) %>% 
  do.call("rbind", .)


d <- d %>% 
  left_join(
    d_meta %>% 
      dplyr::select(series_id, lon, lat), by = "series_id"
  ) %>% 
  mutate(
    air_temp = find_temperature(lon, lat, year, series_id)
  )
d <- d %>% dplyr::select(-c(lon, lat))

write_csv(d_meta, "cleaned_data/mastTree_meta_cleaned.csv")
write_csv(d, "cleaned_data/mastTree_series_cleaned.csv")








