#' Retrieve companion matrix from a Bayesian VAR
#'
#' Calculates the companion matrix for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @inheritParams coef.bvar
#'
#' @return Returns a numeric array/matrix of class \code{bvar_comp} with the
#' VAR's coefficents in companion form at the specified values.
#'
#' @seealso \code{\link{bvar}}; \code{\link{coef.bvar}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom stats quantile
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
#' # Get companion matrices for confidence bands at 10%, 50% and 90%
#' companion(x, conf_bands = 0.10)
#' }
companion <- function(object, ...) {UseMethod("companion", object)}


#' @noRd
companion.default <- function(object, ...) {
  stop("No methods for class ",
    paste0(class(object), collapse = " / "), " found.")
}


#' @rdname companion
#' @export
companion.bvar <- function(
  object,
  type = c("quantile", "mean"),
  conf_bands = 0.5,
  ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  type <- match.arg(type)

  K <- object[["meta"]][["K"]]
  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]
  vars <- name_deps(object[["variables"]], M = M)
  vars_expl <- name_expl(vars, M = M, lags = lags)[-1] # No constant
  vars_dep <- c(vars, if(lags > 1) {rep("lag", M * (lags - 1))})

  if(type == "quantile") {
    quantiles <- quantile_check(conf_bands)
    coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)
  } else {
    quantiles <- 0.5
    coefs <- apply(object[["beta"]], c(2, 3), mean)
  }

  if(length(quantiles) == 1) {
    comp <- get_beta_comp(coefs, K, M, lags)
    dimnames(comp) <- list(vars_dep, vars_expl)
  } else {
    comp <- array(NA, c(length(quantiles), K - 1, K - 1))
    for(i in 1:length(quantiles)) {
      comp[i, , ] <- get_beta_comp(coefs[i, , ], K, M, lags)
    }
    dimnames(comp)[[1]] <- dimnames(coefs)[[1]]
    dimnames(comp)[[2]] <- vars_dep
    dimnames(comp)[[3]] <- vars_expl
  }

  class(comp) <- append("bvar_comp", class(comp))

  return(comp)
}


#' @export
print.bvar_comp <- function(x, digits = 3L, complete = FALSE, ...) {

  if(!inherits(x, "bvar_comp")) {stop("Please provide a `bvar_comp` object.")}

  .print_coefs(x, digits, type = "companion", complete = complete, ...)

  return(invisible(x))
}
