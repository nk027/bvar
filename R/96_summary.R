#' @rdname bvar
#' @export
summary.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  coef_x <- coef(x)
  vcov_x <- vcov(x)
  logLik_x <- logLik(x)

  out <- list(
    "bvar" = x,
    "coef" = coef_x,
    "vcov" = vcov_x,
    "logLik" = logLik_x
  )
  class(out) <- "bvar_summary"

  return(x)
}


#' @rdname summary.bvar
#' @export
print.bvar_summary <- function(x, ...) {
  
  print(x[["bvar"]])
  cat("\n"); print(x[["coef"]])
  cat("\n"); print(x[["vcov"]])
  cat("\n"); cat("Log-Likelihood: ", x[["logLik"]])

  return(invisible(x))
}
