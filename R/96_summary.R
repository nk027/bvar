#' @rdname bvar
#' @export
summary.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  coef_x <- coef(x)
  vcov_x <- vcov(x)
  logLik_x <- logLik(x)

  print(x)
  cat("\n"); print(coef_x)
  cat("\n"); print(vcov_x)
  cat("\n"); cat("Log-Likelihood: ", logLik_x)

  return(invisible(x))
}
