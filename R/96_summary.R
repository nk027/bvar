#' Summary method for Bayesian VARs
#'
#' Retrieves several outputs of interest, including the median coefficient
#' matrix, the median variance-covariance matrix, and the log-likelihood.
#' Separate summary methods exist for impulse responses and forecasts.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Not used.
#'
#' @return Returns a list of class \code{bvar_summary} with elements that can
#' can be accessed individually:
#' \itemize{
#'   \item \code{bvar} - the \code{bvar} object provided.
#'   \item \code{coef} - coefficient values from \code{\link{coef.bvar}}.
#'   \item \code{vcov} - VCOV values from \code{\link{vcov.bvar}}.
#'   \item \code{logLik} - the Log-Likelihood from \code{\link{logLik.bvar}}.
#' }
#'
#' @seealso \code{\link{coef.bvar}}; \code{\link{logLik.bvar}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#' summary(x)
#' }
summary.bvar <- function(object, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  out <- list(
    "bvar" = object,
    "coef" = coef(object),
    "vcov" = vcov(object),
    "logLik" = logLik(object))
  class(out) <- "bvar_summary"

  return(out)
}


#' @export
print.bvar_summary <- function(x, ...) {

  print(x[["bvar"]])

  cat("\n"); print(x[["coef"]])
  cat("\n"); print(x[["vcov"]])
  cat("\n"); cat("Log-Likelihood:", x[["logLik"]], "\n")

  return(invisible(x))
}
