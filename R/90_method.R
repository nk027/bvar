#' BVAR print methods
#'
#' Print methods for \code{bvar} objects providing a quick overview of results.
#' The method for impulse response functions provides information on
#' identification, horizon, et cetera; The forecast method on the horizon.
#'
#' @param x A \code{bvar} / \code{bvar_...} object, obtained from
#' \code{\link{bvar}} or related functions.
#' @param ... Not used.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Print various output generated
#' print(x)
#' }
print.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  cat("Bayesian VAR consisting of", x$meta$N, "observations,",
      x$meta$M, "variables and", x$meta$lags, "lags.")
  cat("\nTime spent calculating:", format(round(x$meta$timer, 2)))
  cat("\nHyperparameters:",
      paste(x$priors$hyper, collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x$optim$par, 3), collapse = ", "))
  cat("\nIterations (burnt / thinning): ", x$meta$n_draw, " (", x$meta$n_burn,
      " / ", x$meta$n_thin, ")", sep = "")
  cat("\nAccepted draws (rate): ", x$meta$accepted, " (",
      round(x$meta$accepted / (x$meta$n_draw - x$meta$n_burn), 3),
      ")\n", sep = "")

  if(!is.null(x$irf)) {cat("\n"); print(x$irf)}
  if(!is.null(x$fcast)) {cat("\n"); print(x$fcast)}

  return(invisible(x))
}
