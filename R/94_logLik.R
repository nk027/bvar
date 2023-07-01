
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
logLik.bvar <- function(object, type = c("mean", "quantile"), ...) {

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  mean <- fitted(object, type = type)[]
  sigma <- vcov(object, type = type)[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  attr(ll, "nall") <- N
  attr(ll, "nobs") <- N
  attr(ll, "df") <- K # Maybe provide effective DoF
  class(ll) <- "logLik"

  return(ll)
}

# To-do: WAIC
#' @export
WAIC <- function(x, ...) {UseMethod("WAIC", x)}


#' @noRd
WAIC.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @noRd
WAIC.bvar <- function(x, n_thin = 1L, ...) {

  Y <- x[["meta"]][["Y"]]
  N <- x[["meta"]][["N"]]
  K <- x[["meta"]][["K"]]

  n_pres <- x[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
    "Issue with n_thin. Maximum allowed is n_save / 10.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  # Matrix of log-likelihoods
  ll <- matrix(NA_real_, N, n_save)
  for(s in seq_len(n_save)) {
    mean <- x[["meta"]][["X"]] %*% x[["beta"]][s, , ]
    sigma <- x[["sigma"]][s, , ]
    ll[, s] <- vapply(seq_len(N), function(i) {
      dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
    }, numeric(1L))
  }

  # WAIC
  lppds <- log(rowMeans(exp(ll))) # log pointwise predictive densities
  res <- ll - matrix(rowMeans(ll), N, n_save)
  pWAIC <- sum(rowMeans(res * res) * (N / (N - 1))) # Gelman (2014)
  WAIC <- -2 * sum(lppd) + 2 * pWAIC

  return(WAIC)
}
