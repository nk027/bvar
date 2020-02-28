#' Retrieve companion matrix from a Bayesian VAR
#'
#' Calculates the companion matrix for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. \code{0.5} is always included.
#' @param ... Not used.
#'
#' @return Returns a numeric array/matrix of class \code{bvar_comp} with the
#' VAR's coefficents in companion form, at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}; \code{\link{coef.bvar}}
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
#' companion(x, conf_bands = 0.10)
#' }
companion <- function(object, ...) {UseMethod("companion", object)}


#' @noRd
companion.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @rdname companion
#' @export
companion.bvar <- function(
  object,
  conf_bands = 0.5,
  ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  K <- object[["meta"]][["K"]]
  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]
  vars <- name_deps(object[["variables"]], M = M)
  vars_expl <- name_expl(vars, M = M, lags = lags)
  vars_dep <- c(vars, name_expl(vars, M = M, lags = lags - 1)[-1])

  quantiles <- quantile_check(conf_bands)
  coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)

  if(length(quantiles) == 1) {
    comp <- get_beta_comp(coefs, K, M, lags)
    dimnames(comp) <- list(vars_dep, vars_expl[-1])
  } else {
    comp <- array(NA, c(length(quantiles), K - 1, K - 1))
    for(i in 1:length(quantiles)) {
      comp[i, , ] <- get_beta_comp(coefs[i, , ], K, M, lags)
    }
    dimnames(comp)[[1]] <- dimnames(coefs)[[1]]
    dimnames(comp)[[2]] <- vars_dep
    dimnames(comp)[[3]] <- vars_expl[-1]
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
