hypothesis_draws <- function(..., 
                             hypothesis, 
                             class = "b",
                             group = "",
                             scope = c("standard", "ranef", "coef"),
                             alpha = 0.05,
                             robust = FALSE,
                             seed = NULL){
  nms <- sapply(substitute(list(...))[-1], deparse)
  
  list(...) %>% 
    purrr::map2(nms,function(x, y){
      df <- brms::hypothesis(x, 
                             hypothesis = hypothesis, 
                             class = class,
                             group = group,
                             alpha = alpha, 
                             scope = scope,
                             robust = robust,
                             seed = seed)$samples
      cbind(df, "model" = y)
    }) %>% 
    do.call("rbind", .) 
}


compute_r2 <- function(y, x, beta, trans = identity){
  z <- outer(x, beta)
  y_hat <- trans(z)
  
  matrixStats::colVars(y_hat) / var(y)
}
