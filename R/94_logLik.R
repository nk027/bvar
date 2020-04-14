#' Log-Likelihood method for Bayesian VARs
#'
#' Calculates the log-likelihood of Bayesian VAR models generated with
#' \code{\link{bvar}}.
#'
#' @author Nikolas Kuschnig, Florian Huber
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
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
#' @noRd
logLik.bvar <- function(object, ...) { # Todo: Add conf_bands

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  mean <- fitted(object, conf_bands = 0.5)
  sigma <- vcov(object, conf_bands = 0.5)[]

  ll <- sum(vapply(seq_len(N), function(i) {
    dmvnorm(Y[i, ], mean[i, ], sigma, log = TRUE)
  }, numeric(1)))

  # attr(ll, "df") <- NA # Maybe provide effective DoF
  attr(ll, "nall") <- N
  class(ll) <- "logLik"

  return(ll)
}
