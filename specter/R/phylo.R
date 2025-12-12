match_family <- function(genus, taxon = "plant"){
  df <- rtrees::classifications %>% 
    dplyr::filter(taxon == taxon)
  df$family[match(genus, df$genus)]
}
