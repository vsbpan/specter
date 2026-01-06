# match_family <- function(genus, taxon = "plant"){
#   df <- rtrees::classifications %>% 
#     dplyr::filter(taxon == taxon)
#   df$family[match(genus, df$genus)]
# }

# Convert ott_id from open tree of life to NCBI id
ott2ncbi <- function(x){
  vapply(x, function(w){
    res <- rotl::taxon_external_IDs(w)
    res <- res[(res[,"source"] == "ncbi"),"id"]
    
    if(is.null(res) || any(is.na(res)) || length(res) == 0){
      return(NA_real_)
    } else {
      if(length(res) != 1){
        # Pick the first id if multiple are returned
        res <- res[1]
      }
      return(as.numeric(res))
    }
  }, numeric(1)) 
}

# Convert ott_id from open tree of life to GBIF id
ott2gbif <- function(x){
  vapply(x, function(w){
    res <- rotl::taxon_external_IDs(w)
    res <- res[(res[,"source"] == "gbif"),"id"]
    
    if(is.null(res) || any(is.na(res)) || length(res) == 0){
      return(NA_real_)
    } else {
      if(length(res) != 1){
        # Pick the first id if multiple are returned
        res <- res[1]
      }
      return(as.numeric(res))
    }
  }, numeric(1)) 
}

# Convert ott_id from open tree of life to ITIS id
ott2itis <- function(x){
  taxadb::get_ids(taxadb::get_names(x, "ott"), provider = "itis")
}

# Convert itis id to taxon info at a particular rank
itis2rank_at <- function(x, rank){
  res <- taxadb::filter_id(x)[,c(rank, "taxonID"), drop = FALSE]
  left_join(
    data.frame("taxonID" = as.character(x)),
    res,
    by = "taxonID"
  )[,rank, drop = TRUE]
}


# Convert NCBI id to taxon info at a particular rank
ncbi2rank_at <- function(x, rank){
  res <- taxizedb::taxa_at(x,rank = rank, db='ncbi')
  res <- keep_rowname(do.call("rbind", res), "id2")
  left_join(
    data.frame("id2" = as.character(x)),
    res,
    by = "id2"
  )$name
}

# Convert GBIF id to taxon info at a particular rank
gbif2rank_at <- function(x, rank){
  res <- taxizedb::taxa_at(x,rank = rank, db='gbif')
  res <- keep_rowname(do.call("rbind", res), "id2")
  left_join(
    data.frame("id2" = as.character(x)),
    res,
    by = "id2"
  )$name
}

# Convert itis id to taxon info at the specific ranks
itis2taxonomy <- function(x, ranks = c("genus", "family","order", "class", "phylum", "kingdom")){
  id2taxonomy(x, ranks, id_type = "itis")
}

# Convert NCBI id to taxon info at the specific ranks
ncbi2taxonomy <- function(x, ranks = c("genus", "family","order", "class", "phylum", "kingdom")){
  id2taxonomy(x, ranks, id_type = "ncbi")
}

# Convert GBIF id to taxon info at the specific ranks
gbif2taxonomy <- function(x, ranks = c("genus", "family","order", "class", "phylum", "kingdom")){
  id2taxonomy(x, ranks, id_type = "gbif")
}

# Convert GBIF id to taxon info at the specific ranks
id2taxonomy <- function(x, ranks = c("genus", "family","order", "class", "phylum", "kingdom"), 
                        id_type = c("ncbi", "gbif", "itis")){
  f <- switch(match.arg(id_type), 
              "ncbi" = ncbi2rank_at,
              "gbif" = gbif2rank_at,
              "itis" = itis2rank_at)
  lapply(ranks, function(r){
    f(x, r)
  }) %>% 
    do.call("cbind", .) %>% 
    as.data.frame() %>% 
    setNames(ranks)
}

# Convert NCBI id to rank
ncbi2rank <- function(x){
  out <- x
  out[!is.na(x)] <- taxizedb::taxid2rank(x[!is.na(x)],db = "ncbi")
  out
}

# Convert GBIF id to rank
gbif2rank <- function(x){
  out <- x
  out[!is.na(x)] <- taxizedb::taxid2rank(x[!is.na(x)],db = "gbif")
  out
}

# Convert itis id to rank
itis2rank <- function(x){
  out <- x
  out[!is.na(x)] <- taxadb::filter_id(x[!is.na(x)])$taxonRank
  out
}

# Only works for plants
genus2family <- function(x){
  res <- rtrees::classifications %>% 
    filter(taxon == "plant") %>% 
    filter(genus %in% x)
  left_join(
    data.frame("genus" = as.character(x)),
    res,
    by = "genus"
  )$family
}
