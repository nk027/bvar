#' Log-Likelihood method for Bayesian VARs
#'
#' Calculates the log-likelihood of Bayesian VARs generated with
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns an object of class \code{logLik}.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @importFrom mvtnorm dmvnorm
#' @importFrom stats logLik
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get log-likelihood
#' logLik(x)
#' }
logLik.bvar <- function(object, ...) {

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  mean <- fitted(object)
  sigma <- vcov(object, 0.5)[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  attr(ll, "df") <- K
  attr(ll, "nall") <- N
  class(ll) <- "logLik"

  return(ll)
}
