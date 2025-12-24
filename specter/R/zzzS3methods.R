
# Generic method for checking convergence
converged <- function(x, ...){
  UseMethod("converged")
}

# Generate posterior prediction from model
posterior_epred <- function(x, newdata = NULL, ndraws, 
                            re_formula = NA, 
                            scale = c("response", "link"), 
                            cpp = FALSE,
                            ...) {
  if(is.error(x)){
    cli::cli_alert_danger("Returning NAs. Model has error.")
    if(is.null(newdata)){
      stop("Model is error and has no rows. Maybe try suppling 'newdata' or fix the error?")
    } else {
      n <- nrow(newdata)
    }
    preds <- matrix(rep(NA_real_, prod(ndraws, n)), 
                    nrow = ndraws, ncol = n)
    rownames(preds) <- paste0("draw_", seq_len(ndraws))
    return(preds)
  }
  UseMethod("posterior_epred")
}

registerS3method(genname = "posterior_epred", 
                 class = "mle_fit", 
                 method = posterior_epred.mle_fit)

registerS3method(genname = "coef", 
                 class = "mle_fit", 
                 method = coef.mle_fit)

registerS3method(genname = "converged", 
                 class = "mle_fit", 
                 method = converged.mle_fit)

registerS3method(genname = "summary",
                 class = "mle_fit",
                 method = summary.mle_fit)

registerS3method(genname = "print",
                 class = "mle_fit",
                 method = print.mle_fit)

registerS3method(genname = "vcov", 
                 class = "mle_fit", 
                 method = vcov.mle_fit)

registerS3method(genname = "plot", 
                 class = "series", 
                 method = plot.series)

registerS3method(genname = "print",
                 class = "distr",
                 method = print.distr)