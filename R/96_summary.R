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
#'   \item \code{logLik} - the log-likelihood from \code{\link[stats]{logLik}}.
#' }
#'
#' @seealso \code{\link{bvar}};
#' \code{\link{predict.bvar}}; \code{\link{irf.bvar}}
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

  out <- structure(list(
    "bvar" = object, "coef" = coef.bvar(object), "vcov" = vcov.bvar(object),
    "logLik" = logLik.bvar(object)), class = "bvar_summary")

  return(out)
}


#' @export
print.bvar_summary <- function(x, ...) {

  print(x[["bvar"]])

  cat("\n"); print.bvar_coefs(x[["coef"]])
  cat("\n"); print.bvar_vcovs(x[["vcov"]])
  cat("\n"); cat("Log-Likelihood:", x[["logLik"]], "\n")

  return(invisible(x))
}
