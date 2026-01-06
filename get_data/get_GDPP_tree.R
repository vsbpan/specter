library(rotl)
library(tidyverse)
vmisc::load_all2("specter")

# Used rotl::tnrs_match_names() to search for names
# Also did more manual cleaning
# `taxonname` is the key from the raw GPDD data
# `name_cleaned` is the best name for plotting
# `search_string` is what is used to look up the ott_id
# `unique_name` is what is in the phylo tree. Note that for some, the search string is changed to higher taxanomic levels because no unique_name matached the search string or the found ott_id turns out to be invalid. 
z <- read_csv("cleaned_data/GPDD_taxon_info.csv")

tre <- tol_induced_subtree(ott_ids = unique(na.omit(z$ott_id)))

tre$tip_og_lab <- tre$tip.label
tre$tip.label <- gsub(".*ott","", tre$tip.label)
tre$edge.length <- rep(1, nrow(tre$edge)) # No branch length available, assume each node is unit 1. 
tre <- phytools::force.ultrametric(tre) # Make the present day tips be the same length from the root
tre$tip_og_lab <- rotl::strip_ott_ids(tre$tip_og_lab)

# ape::write.tree(tre, "cleaned_data/GPDD_tree.tre")

tre <- ape::read.tree("cleaned_data/GPDD_tree.tre")
ggtree::ggtree(tre, layout = "circular")
