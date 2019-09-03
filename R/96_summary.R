#' @rdname bvar
#' @export
summary.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  print(x)
  cat("\n"); print(coef(x))
  cat("\n"); print(vcov(x))
  cat("\n"); print(logLik(x))

  return(invisible(x))
}
