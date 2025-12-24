print.distr <- function(x, ...){
  
  v <- mapply(function(x, y){
    paste0(cli::col_yellow(x), "=", y)
  }, names(x$param), x$param, SIMPLIFY = TRUE)
  v <- paste0(v, collapse = ", ")
  w <- if(is.null(x$vcv)) "without" else "with"
  
  cli::cli_text(
    "`distr` object {w} vcv"
  )
  cli::cli_text(
    "{cli::col_blue(x$name)}({v})"
  )
}
