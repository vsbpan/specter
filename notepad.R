

z <- read_csv("cleaned_data/GPDD_taxon_info.csv")

ncbi2taxonomy(z2$ncbi_id) %>% 
  as_tibble() %>% 
  print(n = Inf)

length(x)
ncbi2rank(na.omit(x), "genus") %>% length()


z2 <- z %>% 
  filter(ott_id %in% as.numeric(na.omit(x))) %>% 
  select(
    name_cleaned, ott_id
  ) %>% 
  mutate(
    ncbi_id = ott2ncbi(ott_id)
  )

z2 %>% 
  mutate(
    genus = ncbi2rank(ncbi_id,"genus")
  ) %>% View()


z$ott_id %>% sample(100) -> x

z %>% 
  head() %>% 
  select(
    name_cleaned, ott_id
  ) %>% 
  mutate(
    ncbi_id = ott2ncbi(ott_id)
  ) %>% 
  mutate(
    class = ncbi2rank_at(ncbi_id, "class")
  ) 





series_make(
  1:100,
  rnorm(100),
  "foo"
) %>%
  find_splitted_attributes(split_method = "equ", len = 20)

  series_split(method = "equal", len = 10) %>% 
  lapply(function(x){
    series_calc(x) %>% 
      collect_attributes()
  })

  
  


foo <- terra::rast("cleaned_data/annual_air_temperature_20th_century_reanalysis.nc")

1960:1990 %>% 
    series_make(
      .,
      rnorm(length(.)), 
      ID = "foo"
    ) -> z
  
  
foo
z
  
  
foo %>% terra::time()
  
extract2(foo)
  
terra::extract(foo, data.frame("lon" = c(-33,60), "lat" =  c(20, -3)))
  
extract2(foo, c(-33, 60), c(20, -3), c(1993,1993, NA,1999, 30000, 2015))


find_temperature(c(-33, 60), c(20, -3), c(1993, 1999), rast_source = "reanalysis")




d %>% 
  filter(
    !is.na(sampleyear) & !is.na(population)
  ) %>% 
  mutate(
    series_id = paste0("series_", mainid)
  ) %>% 
  group_by(series_id, sampleyear) %>% 
  summarise(
    population = max(population)
  ) %>% 
  ungroup() %>% 
  left_join(d_meta %>% 
              mutate(
                series_id = paste0("series_", mainid)
              ) %>% 
              select(series_id, locationid) %>% 
              left_join(d_loc,by = "locationid") %>% 
              transmute(
                series_id = series_id,
                lon = longdd,
                lat = latdd
              )) -> w







