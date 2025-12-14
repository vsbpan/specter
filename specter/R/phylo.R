# match_family <- function(genus, taxon = "plant"){
#   df <- rtrees::classifications %>% 
#     dplyr::filter(taxon == taxon)
#   df$family[match(genus, df$genus)]
# }


ott2ncbi <- function(x){
  vapply(x, function(w){
    res <- rotl::taxon_external_IDs(w)
    res <- res[(res[,"source"] == "ncbi"),"id"]
    if(length(res) == 0 || is.na(res)){
      return(NA_real_)
    } else {
      return(as.numeric(res))
    }
  }, numeric(1)) 
}


ncbi2rank_at <- function(x, rank){
  res <- taxizedb::taxa_at(x,rank = rank, db='ncbi')
  res <- keep_rowname(do.call("rbind", res), "id2")
  left_join(
    data.frame("id2" = as.character(x)),
    res,
    by = "id2"
  )$name
}

ncbi2taxonomy <- function(x, ranks = c("genus", "family","order", "class", "phylum", "kingdom")){
  lapply(ranks, function(r){
    ncbi2rank_at(x, r)
  }) %>% 
    do.call("cbind", .) %>% 
    as.data.frame() %>% 
    setNames(ranks)
}

ncbi2rank <- function(x){
  out <- x
  out[!is.na(x)] <- taxizedb::taxid2rank(x[!is.na(x)],db = "ncbi")
  out
}
