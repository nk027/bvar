#' Fitted and residual methods for Bayesian VARs
#'
#' Calculates fitted values / resiudals for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#'
#' @param x Object of class \code{bvar_fitted} / \code{bvar_resid}.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param vars Optional numeric vector. Used to subset the plot to certain
#' variables by position. Defaults to \code{NULL}, i.e. all variables.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns a numeric array of class \code{bvar_fitted} /
#' \code{bvar_resid} with desired values at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @importFrom mvtnorm dmvnorm
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get fitted values and adjust confidence bands to 10%, 50% and 90%
#' fitted(x, conf_bands = 0.10)
#'
#' # Get residuals
#' residuals(x)
#' }
logLik.bvar <- function(x, ...) {

  Y <- x[["meta"]][["Y"]]
  N <- x[["meta"]][["N"]]
  K <- x[["meta"]][["K"]]
  mean <- fitted(x)
  sigma <- vcov(x, 0.5)[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  attr(ll, "df") <- K
  attr(ll, "nall") <- N
  class(ll) <- "logLik"

  return(ll)
}
