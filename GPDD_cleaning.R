library(tidyverse)
vmisc::load_all2("specter")

# Read in raw data
d <- readr::read_csv("raw_data/GPDD_series.csv") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(
    sampleyear = ifelse(sampleyear == -9999, NA_real_, sampleyear)
  )
d_taxa <- readr::read_csv("raw_data/GPDD_taxa.csv") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(-notes)

d_loc <- readr::read_csv("raw_data/GPDD_location.csv") %>% 
  dplyr::rename_all(tolower)

d_meta <- readr::read_csv("raw_data/GPDD_meta.csv") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::left_join(d_taxa) %>% 
  dplyr::mutate(
    sourcedimension = tolower(sourcedimension)
  )

d_pub <- readr::read_csv("raw_data/GPDD_pub.csv") %>% 
  dplyr::rename_all(tolower)


# Some table joinining
d_cleaned <- d_meta %>% 
  dplyr::select(mainid:sourcetransformreference, samplingfrequency) %>% 
  dplyr::left_join(
    d_taxa %>% 
      dplyr::select(taxonid, taxonname, commonname),
     by = "taxonid"
  ) %>% 
  dplyr::left_join(
    d_loc %>% 
      dplyr::select(locationid, exactname, longitudedegrees, latitudedegrees, spatialaccuracy, area, altitude),
    by = "locationid"
  ) %>% 
  dplyr::left_join(
    d_pub %>% 
      dplyr::select(datasourceid:availability),
    by = "datasourceid"
  ) %>% 
  dplyr::rename(
    lon = longitudedegrees,
    lat = latitudedegrees
  )

allowed_dimensions <- c("density", "count", "mean count", "count (estimated)", "mean concentration", "mean density")

d <- d %>% 
  dplyr::filter(
    !is.na(sampleyear) & !is.na(population)
  ) %>% # First pass filter by length >= 8 years
  dplyr::group_by(mainid) %>% 
  dplyr::filter(
    n() >= 8
  ) %>% 
  ungroup()

d_cleaned <- d_cleaned %>% # keep only those dataset that met the first pass screening criteria
  dplyr::filter(mainid %in% unique(d$mainid)) %>% 
  dplyr::filter(sourcedimension %in% allowed_dimensions)


# Clean the series
d <- d %>% 
  dplyr::group_by(mainid, sampleyear) %>% 
  dplyr::summarise(
    population = max(population) # find the maximum at each year if there are multiple entries per year
  ) %>% 
  dplyr::rename(
    year = sampleyear
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(mainid, population, year)

# write_csv(d, "cleaned_data/GPDD_series_cleaned.csv")

# Add taxon info
d_taxon2 <- readr::read_csv("cleaned_data/GPDD_taxon_info.csv") %>% 
  dplyr::select(
    taxonname, ott_id, name_cleaned
  ) %>% 
  dplyr::filter(
    taxonname %in% unique(d_cleaned$taxonname)
  )

d_taxon2 <- d_taxon2 %>% 
  dplyr::mutate(
    ncbi_id = ott2ncbi(ott_id),
    ncbi2taxonomy(ncbi_id),
    taxon_rank = ncbi2rank(ncbi_id)
  )

d_taxon2 <- d_taxon2 %>% 
  filter(
    !(is.na(ncbi_id) | is.na(taxon_rank))
  ) %>% 
  rbind(
    d_taxon2 %>% 
      filter(
        is.na(ncbi_id) | is.na(taxon_rank)
      ) %>% 
      left_join(d_taxa, by = "taxonname") %>% 
      mutate_at(
        .vars = vars(taxonomiclevel:taxonomicgenus),
        function(x){
          ifelse(tolower(x) == "unknown", NA, x)
        }
      ) %>% 
      mutate(
        genus = ifelse(is.na(genus), taxonomicgenus),
        family = ifelse(is.na(family), taxonomicfamily),
        order = ifelse(is.na(order), taxonomicorder),
        class = ifelse(is.na(class), taxonomicclass),
        phylum = ifelse(is.na(phylum), taxonomicphylum),
        taxon_rank = ifelse(is.na(taxon_rank), taxonomiclevel)
      ) %>% 
      mutate(
        kingdom = case_when(
          phylum == "Dinophyta (Pyrrophyta)" ~ "Dinophyceae",
          phylum %in% c("Chordata", "Arthropoda", "Mollusca") ~ "Metazoa"
        )
      ) %>% 
      dplyr::select(taxonname:taxon_rank)
  )

d_cleaned <- d_cleaned %>% 
  dplyr::left_join(
    d_taxon2,
    by = "taxonname"
  )

# write_csv(d_cleaned, "cleaned_data/GPDD_meta_cleaned.csv")












