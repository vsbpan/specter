# Find model parameters from a formula
find_params_formula <- function(formula){
  all.vars(as_oneside(formula))
}

# Find response variable from a formula
find_response_formula <- function(formula){
  as.character(formula)[[2]]
}

# Get variance covariance matrix from a `mle_fit` object
vcov.mle_fit <- function(x){
  x$vcov
}

# Get model coefficients from a `mle_fit` object
coef.mle_fit <- function(x){
  x$par
}

# Match and return the correct density function based on the provided conditional distribution
mle_fit_family <- function(dist = c("gaussian", "binomial","poisson", "beta", "Gamma")){
  res <- switch(match.arg(dist),
                "gaussian" = function(resp, mu, log_sigma){
                  dnorm(resp, mu, sd = exp(log_sigma), log = TRUE)
                }, 
                "binomial" = function(resp, mu, log_sigma){
                  v <- (exp(log_sigma))^2
                  
                  alpha <- mu^2 * ((1 - mu) / v - 1 / mu)
                  beta <- alpha * (1 / mu - 1)
                  
                  dbeta(resp, alpha, beta, log = TRUE)
                }, 
                "beta" = function(resp, mu, log_sigma){
                  v <- (exp(log_sigma))^2
                  
                  alpha <- mu^2 * ((1 - mu) / v - 1 / mu)
                  beta <- alpha * (1 / mu - 1)
                  
                  dbeta(resp, alpha, beta, log = TRUE)
                },
                "poisson" = function(resp, mu, log_sigma){
                  v <- (exp(log_sigma))^2
                  
                  alpha <- mu^2 / v
                  beta <- v / mu
                  dgamma(resp, shape = alpha, rate = beta, log = TRUE)
                },
                "Gamma" = function(resp, mu, log_sigma){
                  v <- (exp(log_sigma))^2
                  
                  alpha <- mu^2 / v
                  beta <- v / mu
                  dgamma(resp, shape = alpha, rate = beta, log = TRUE)
                })
  return(res)
}

# Find the family specific auxiliary parameters
find_family_param <- function(dist = c("gaussian", "beta", "Gamma")){
  switch(
    match.arg(dist), 
    "gaussian" = "log_sigma", 
    "beta" = "log_sigma", 
    "Gamma" = "log_sigma"
  )
}

# Use maximum likelihood estimation or maximum a posteriori to estimate parameters in an objective function
mle_fit <- function(fn, data, start, prior = NULL, dist = c("gaussian", "beta", "Gamma"), ..., empty = FALSE){
  dist <- match.arg(dist)
  f <- as.function(fn)
  all_params <- find_params_formula(fn)
  param_names <- all_params[!all_params %in% names(data)]
  predictor_names <- all_params[all_params %in% names(data)]
  dist <- match.arg(dist)
  param_names <- c(param_names, find_family_param(dist))
  predictors <- as.list(data[,predictor_names, drop = FALSE])
  resp_name <- find_response_formula(fn)
  resp <- data[, resp_name, drop = TRUE]
  n <- length(param_names)
  ld_fun <- mle_fit_family(dist)
  link_fun <- match_link_fun(dist)
  
  if(is.null(prior)){
    nll <- function(theta){
      farg <- as.list(theta)[-n]
      names(farg) <- param_names[-n]
      farg <- c(
        farg, 
        predictors
      )
      
      y_hat <- do.call(f, farg)
      
      -sum(
        ld_fun(resp, mu = link_fun(y_hat), theta[n])
      )
    } 
  } else {
    prior <- prior[match(names(prior), param_names)]
    prior_log_density <- make_prior(prior)
    
    nll <- function(theta){
      farg <- as.list(theta)[-n]
      names(farg) <- param_names[-n]
      farg <- c(
        farg, 
        predictors
      )
      
      y_hat <- do.call(f, farg)
      
      -sum(
        ld_fun(resp, link_fun(y_hat), theta[n])
      ) - prior_log_density(theta)
    }
  }
  
  init <- unlist(start[match(names(start), param_names)])
  if(length(init) != length(param_names)){
    stop(sprintf("Initial parameters not the same length as the number of parameters: %s", 
                 paste0(param_names, collapse = ", ")))
  }
  
  if(isTRUE(empty)){
    fit <- list(
      "par" = setNames(rep(NA_real_, n), param_names),
      "value" = NA_real_,
      "counts" = setNames(c(NA_integer_, NA_integer_), c("function", "gradient")),
      "convergence" = 3L,
      "message" = "empty object with mle_fit(empty = TRUE)",
      "vcov" = matrix(rep(NA_real_, n^2), dimnames = list(param_names, param_names), nrow = n, ncol = n)
    )
  } else {
    fit <- calibrar::optim2(init, fn = nll, hessian = TRUE, ...)
    
    if(is.null(fit$hessian)){
      fit$hessian <- hessian(nll, fit$par)
      dimnames(fit$hessian) <- list(names(fit$par), names(fit$par))
    }
    
    fit$vcov <- tryCatch(solve(fit$hessian), error = function(e){
      message(e$message)
      
      h_dim <- dim(fit$hessian)
      
      return(
        matrix(rep(NA_real_, prod(h_dim)), 
               dimnames = dimnames(fit$hessian), 
               nrow = h_dim[1], 
               ncol = h_dim[2])
      )
    })
    diag(fit$vcov)[diag(fit$vcov) < 0] <- NA_real_
    not_pos_def <- tryCatch(any(eigen(fit$vcov, only.values = TRUE)$values < 0), 
                            error = function(e){
                              FALSE
                            })
    if(isTRUE(not_pos_def)){
      cli::cli_warn("Variance-covariance matrix is not positive semidefinite.")
      fit$vcov[] <- NA_real_
    }
    fit$hessian <- NULL
  }
  
  fit$se <- suppressWarnings(sqrt(diag(fit$vcov)))
  
  if(isTRUE(fit$convergence == 0) && any(is.na(fit$vcov))){
    fit$convergence <- 2 
  }
  fit$init <- init
  fit$formula <- fn
  fit$data <- data
  fit$loglik <- -fit$value
  fit$prior <- prior
  fit$dist <- dist
  fit$n <- nrow(data)
  fit$y_name <- resp_name
  fit$obj_method <- if(is.null(prior)) c("MLE") else c("MAP")
  
  class(fit) <- if(is.null(prior)) c("mle_fit") else c("map_fit","mle_fit")
  return(fit)
}

# Print method for a `mle_fit` object
print.mle_fit <- function(x, digits = 4){
  cli::cat_line("Formula: ")
  cli::cat_print(drop_attributes(unclass(x$formula)))
  cat("\n")
  if(converged(x)){
    cli::cli_alert_success("Model convergence success!")
  } else {
    cli::cli_alert_danger(sprintf("Model failed to converge: %s", x$convergence))
  }
  cli::cat_line(sprintf("Family: %s (%s)", x$dist, x$obj_method))
  cli::cat_line(sprintf("Loglik: %s (n=%s)\n", round(x$loglik, 4), x$n))
  print(summary(x), digits = digits)
}

# Summary method for a `mle_fit` object
summary.mle_fit <- function(x){
  data.frame(
    "estimate" = unname(x$par),
    "se" = unname(x$se), 
    row.names = names(x$par)
  )
}


# Method to simulate predictions from a fitted mle model
#' @export
posterior_epred.mle_fit <- function(x, newdata = NULL, ndraws = 100, re_formula = NA, 
                                    scale = c("response", "link"), cpp = FALSE, ...){
  vcov_mat <- tryCatch(vcov(x), error = function(e) {
    message("Error in computing the variance-covariance matrix: ")
    message(e$message)
    return(NULL)
  })
  if (is.null(vcov_mat) || is.error(vcov_mat) || any(is.na(vcov_mat))) {
    cli::cli_alert_danger("Returning NAs. Cannot retreive variance covariance matrix.")
    preds <- matrix(rep(NA_real_, prod(ndraws, nrow(newdata))), 
                    nrow = ndraws, ncol = nrow(newdata))
  }
  else {
    coefs <- tryCatch({
      mvnfast::rmvn(ndraws, mu = stats::coef(x), sigma = vcov_mat, 
                    kpnames = TRUE)
    }, error = function(e){
      message("Error in sampling from multivariate normal distribution. Falling back to slower method. ")
      message(e$message)
      mvtnorm::rmvnorm(ndraws, mean = stats::coef(x),sigma = vcov_mat)
    })
    
    if(is.null(newdata)){
      newdata <- x$data
    }
    
    stopifnot(is.data.frame(newdata))
    newdata <- as.list(newdata)
    FUN <- as.function(x$formula)
    preds <- lapply(seq_len(ndraws), 
                    function(i) {
                      do.call(FUN, c(coefs[i,],newdata))
                    }) %>% 
      do.call("rbind", .)
  }
  rownames(preds) <- paste0("draw_", seq_len(ndraws))
  return(preds)
}

# Outputs a function that takes theta and outputs the log prior density
make_prior <- function(prior){
  f <- function(theta){
    v <- lapply(
      seq_along(prior), 
      function(i){
        # Must be a named list
        x <- prior[[i]]
        arg <- as.list(x$param)
        arg <- c(theta[i],arg)
        names(arg)[1] <- "x"
        log(do.call(paste0("d", x$name), arg))
      }
    )
    sum(do.call("c", v))
  }
  return(f)
}

# Turn two sided formula into one sided formula
as_oneside <- function(formula){
  as.formula(paste0("~ ", as.character(formula)[3]))
}


# Check whether the model has converged
converged.mle_fit <- function(x){
  isTRUE(x$convergence == 0)
}

match_link_fun <- function(dist){
  switch(dist, 
         "poisson" = function(x) log(x),
         "Gamma" = function(x) log(x), 
         "gaussian" = function(x) x,
         "binomial" = function(x) log(x), 
         "beta" = function(x) log(x))
}


update_params <- function(x, old_param, new_param, trans){
  stopifnot(
    length(old_param) == length(new_param)
  )
  stopifnot(
    length(trans) == length(new_param)
  )
  
  old_names <- names(coef(x))
  
  stopifnot(
    length(old_names) == length(old_param)
  )
  
  o <- match(old_param,old_names)
  
  stopifnot(
    !any(is.na(o))
  )
  
  trans <- trans[o]
  new_param <- new_param[o]
  
  x$vcov_og <- x$vcov
  x$par_og <- x$par
  
  x$vcov <- vmisc::FOSM2(
    coef(x), 
    vcov(x), 
    trans = trans
  )
  dimnames(x$vcov) <- list(new_param, new_param)
  x$se <- sqrt(diag(x$vcov))
  x$par <- do.call("c", purrr::map2(
    x$par,
    trans,
    function(a,b){
      b(a)
    }
  ))
  names(x$par) <- new_param
  x
}


predict.mle_fit <- function(object, ci = FALSE, ...){
  mat <- posterior_epred(object, ...)
  
  mu <- matrixStats::colMeans2(mat)
  if(isTRUE(is.numeric(ci))){
    ci_l <- (1 - ci) / 2
    ci_u <- ci + ci_l
    out <- as.data.frame(cbind("estimate" = mu, matrixStats::colQuantiles(mat, probs = c(ci_l, ci_u))))
    names(out)[2:3] <- c("lower", "upper")
  } else {
    out <- mu
  }
  return(out)
  
}


r2.mle_fit <- function(model, ...){
  y <- model$data[,model$y_name]
  x <- predict(model)
  cor(x, y)^2
}
