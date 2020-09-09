
#' Log-Likelihood method for Bayesian VARs
#'
#' Calculates the log-likelihood of Bayesian VAR models generated with
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Not used.
#'
#' @return Returns an object of class \code{logLik}.
#'
#' @seealso \code{\link{bvar}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom mvtnorm dmvnorm
#' @importFrom stats logLik
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
#'
#' # Calculate the log-likelihood
#' logLik(x)
#' }
logLik.bvar <- function(object, ...) { # Todo: Maybe add conf_bands

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  mean <- fitted(object, conf_bands = 0.5)
  sigma <- vcov(object, conf_bands = 0.5)[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  attr(ll, "nall") <- N
  attr(ll, "nobs") <- N
  attr(ll, "df") <- K # Maybe provide effective DoF
  class(ll) <- "logLik"

  return(ll)
}
