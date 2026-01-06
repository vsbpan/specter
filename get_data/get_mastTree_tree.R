library(rotl)
library(tidyverse)
vmisc::load_all2("specter")

d_meta <- suppressMessages(read_csv("cleaned_data/mastTree_meta_cleaned.csv"))

tre <- rtrees::get_tree(
  d_meta %>% 
    transmute(
      species = taxonname,
      genus = ifelse(is.na(genus), "missing", genus), 
      family = family
    ) %>% 
    unique(), 
  taxon = "plant")

ape::write.tree(tre, "cleaned_data/mastTree_tree.tre")






