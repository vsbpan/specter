pipeline_wrapper <- function(series){
  res <- series %>% 
    series_whole_attr() %>% 
    series_split() %>% 
    lapply(function(x){
      series_calc(x) %>% 
        collect_attributes()
    }) %>% 
    do.call("rbind", .)
  rownames(res) <- NULL
  res
}













