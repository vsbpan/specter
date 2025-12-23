assign_chunk <- function(.df, n_chunks = 32, group = NULL){
  if(is.null(group)){
    n <- nrow(.df)
    s <- ceiling(n/n_chunks)
    
    .df$chunk_id <- sample(
      rep(paste0("chunk", seq_len(n_chunks)), s),
      n, 
      replace = FALSE
    ) 
  } else {
    u <- unique(.df[[group]])
    n <- length(u)
    s <- ceiling(n/n_chunks)
    
    i <- sample(
      rep(paste0("chunk", seq_len(n_chunks)), s),
      n, 
      replace = FALSE
    ) 
    
    .df$chunk_id <- i[match(.df[[group]], u)]
  }
  .df
}

# Remove attributes except for those specified in `exclude =` 
drop_attributes <- function(x, exclude = NULL){
  if(is.null(exclude)){
    attributes(x) <- NULL
  } else {
    attributes(x) <- attributes(x)[exclude]
  }
  return(x)
}