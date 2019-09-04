#' Retrieve companion matrix from a Bayesian VAR
#'
#' Calculates the companion matrix for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is included as a default.
#' @param complete Logical value. Whether to retrieve the companion matrix for
#' all saved draws of the VAR coefficients. Overrides \code{conf_bands} if set
#' \code{TRUE}.
#'
#' @return Returns a numeric array/matrix of class \code{bvar_comp} containing
#' the companion matrix of the VARs' coefficients with desired values at the
#' specified confidence bands.
#'
#' @seealso \code{\link{bvar}}, \code{\link{get_beta_comp}}
#'
#' @export
#'
#' @importFrom stats
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' object <- bvar(data, lags = 2)
#'
#' # Get companion matrices for confidence bands set to 10%, 50% and 90%
#' companion(object, conf_bands = 0.10, complete = FALSE)
#'
#' # Get companion matrices for all draws of the VAR coefficients
#' companion(object, complete = TRUE)
#' }
companion <- function(object, conf_bands = 0.5, complete = FALSE) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  K <- object[["meta"]][["K"]]
  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]

  if(complete) {
    n_save <- object[["meta"]][["n_save"]]
    comp <- array(NA, c(n_save, K - 1, K - 1))
    for(i in 1:n_save) {
      comp[i, , ] <- get_beta_comp(object[["beta"]][i, , ], K, M, lags)
    }
  } else {
    quantiles <- quantile_check(conf_bands)
    beta_quants <- apply(object[["beta"]], c(2, 3), quantile, quantiles)
    has_quants <- length(quantiles) != 1
    if(has_quants) {
      comp <- array(NA, c(length(quantiles), K - 1, K - 1))
      for(i in 1:length(quantiles)) {
        comp[i, , ] <- get_beta_comp(beta_quants[i, , ], K, M, lags)
      }
    } else {
      comp <- get_beta_comp(beta_quants, K, M, lags)
    }
  }
  class(comp) <- "bvar_comp"

  return(comp)
}
