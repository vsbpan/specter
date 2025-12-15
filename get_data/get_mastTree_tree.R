library(rotl)
library(tidyverse)
vmisc::load_all2("specter")

# Used rotl::tnrs_match_names(z$search_string, context_name = "Seed plants") to search for names
# Also did more manual cleaning
# `species` is the key from the raw mastTree data
# `name_cleaned` is the best name for plotting
# `search_string` is what is used to look up the ott_id
# `unique_name` is what is in the phylo tree. Note that for some, the search string is changed to higher taxanomic levels because no unique_name matached the search string or the found ott_id turns out to be invalid. 

z <- read_csv("cleaned_data/mastTree_taxon_info.csv")
tre <- tol_induced_subtree(ott_ids = unique(na.omit(z$ott_id)))

# ape::write.tree(tre, "cleaned_data/mastTree_tree.tre")

tre <- ape::read.tree("cleaned_data/mastTree_tree.tre")
ggtree::ggtree(tre, layout = "circular")








