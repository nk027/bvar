#' Retrieve companion matrix from a Bayesian VAR
#'
#' Calculates the companion matrix for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. \code{0.5} is always included.
#' @param complete Logical value. Whether to retrieve the companion matrix for
#' all saved draws of the VAR coefficients. Overrides \code{conf_bands} if set
#' \code{TRUE}.
#' @param ... Not used.
#'
#' @return Returns a numeric array/matrix of class \code{bvar_comp} with the
#' VAR's coefficents in companion form, at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}; \code{\link{coef}}
#'
#' @export
#'
#' @importFrom stats quantile
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get companion matrices for confidence bands at to 10%, 50% and 90%
#' companion(x, conf_bands = 0.10, complete = FALSE)
#'
#' # Get companion matrices for all draws of the VAR coefficients
#' companion(x, complete = TRUE)
#' }
companion.bvar <- function(
  object,
  conf_bands = 0.5, complete = FALSE,
  ...) {

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
  } else { # !complete
    quantiles <- quantile_check(conf_bands)
    coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)

    has_quants <- length(quantiles) != 1
    if(has_quants) {
      comp <- array(NA, c(length(quantiles), K - 1, K - 1))
      for(i in 1:length(quantiles)) {
        comp[i, , ] <- get_beta_comp(coefs[i, , ], K, M, lags)
      }
    } else {
      comp <- get_beta_comp(coefs, K, M, lags)
    }
  }
  class(comp) <- append("bvar_comp", class(comp))

  return(comp)
}


#' @rdname companion.bvar
#' @export
companion <- function(object, ...) {UseMethod("companion", object)}
