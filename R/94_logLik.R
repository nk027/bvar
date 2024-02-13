
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
logLik.bvar <- function(object, ...) {

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  mean <- fitted.bvar(object, type = "mean")[]
  sigma <- vcov.bvar(object, type = "mean")[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  attr(ll, "nall") <- N
  attr(ll, "nobs") <- N
  attr(ll, "df") <- K # Maybe provide effective DoF
  class(ll) <- "logLik"

  return(ll)
}

#' Widely applicable information criterion (WAIC) for Bayesian VARs
#'
#' Calculates the widely applicable (or Watanabe-Akaike) information criterion
#' (Watanabe, 2010) for VAR models generated with \code{\link{bvar}}. The
#' result equals \deqn{-2 (\text{lppd} - \text{pWAIC}}, where 'lppd' is the
#' log pointwise predictive density, and 'pWAIC' is the effective number of
#' parameters.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{x} is used
#' to calculate, others are dropped.
#' @param ... Not used.
#'
#' @return Returns a numerical value.
#'
#' @references
#'   Watanabe, S. (2010) Asymptotic Equivalence of Bayes Cross Validation and
#'   Widely Applicable Information Criterion in Singular Learning Theory.
#'   \emph{Journal of Machine Learning Research}, \bold{11}, 3571-3594.
#'
#'   Kuschnig, N. and Vashold, L. (2021) BVAR: Bayesian Vector Autoregressions
#'   with Hierarchical Prior Selection in R.
#'   \emph{Journal of Statistical Software}, \bold{14}, 1-27,
#'   \doi{10.18637/jss.v100.i14}.
#' @seealso \code{\link{bvar}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom mvtnorm dmvnorm
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 600L, n_burn = 100L, verbose = FALSE)
#'
#' # Calculate the log-likelihood
#' WAIC(x)
#' }
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
  lppd <- log(rowMeans(exp(ll))) # log pointwise predictive densities
  res <- ll - matrix(rowMeans(ll), N, n_save)
  pWAIC <- sum(rowMeans(res * res) * (N / (N - 1))) # Gelman (2014)
  WAIC <- -2 * sum(lppd) + 2 * pWAIC

  return(WAIC)
}


#' @rdname WAIC.bvar
#' @export
WAIC <- function(x, ...) {UseMethod("WAIC", x)}


#' @rdname WAIC.bvar
#' @export
WAIC.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}
