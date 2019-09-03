#' @rdname bvar
#' @export
summary.bvar <- function(object, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  coef_x <- coef(object)
  vcov_x <- vcov(object)
  logLik_x <- logLik(object)

  irf_x <- if(!is.null(object$irf)) {summary(object$irf)} else {NULL}
  fcast_x <- if(!is.null(object$fcast)) {summary(object$fcast)} else {NULL}

  out <- list(
    "bvar" = object,
    "coef" = coef_x,
    "vcov" = vcov_x,
    "logLik" = logLik_x,
    "fcast" = fcast_x,
    "irf" = irf_x
  )
  class(out) <- "bvar_summary"

  return(out)
}


#' @rdname summary.bvar
#' @export
print.bvar_summary <- function(x, ...) {
  
  print(x[["bvar"]])
  print(x[["fcast"]])
  print(x[["irf"]])
  
  cat("\n"); print(x[["coef"]])
  cat("\n"); print(x[["vcov"]])
  cat("\n"); cat("Log-Likelihood:", x[["logLik"]], "\n")

  return(invisible(x))
}
