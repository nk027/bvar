
#' Coefficient and VCOV methods for Bayesian VARs
#'
#' Retrieves coefficient / variance-covariance values from Bayesian VAR models
#' generated with \code{\link{bvar}}. Note that coefficients are available for
#' every stored draw and one may retrieve (a) credible intervals via the
#' \emph{conf_bands} argument, or (2) means via the \emph{type} argument.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param type Character scalar. Whether to return quantile or mean values.
#' Note that \emph{conf_bands} is ignored for mean values.
#' @param conf_bands Numeric vector of confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#' @param companion Logical scalar. Whether to retrieve the companion matrix of
#' coefficients. See \code{\link{companion.bvar}}.
#' @param ... Not used.
#'
#' @return Returns a numeric array of class \code{bvar_coefs} or
#' \code{bvar_vcovs} at the specified values.
#'
#' @seealso \code{\link{bvar}}; \code{\link{companion.bvar}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom stats coef vcov
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
#' # Get coefficent values at the 10%, 50% and 90% quantiles
#' coef(x, conf_bands = 0.10)
#'
#' # Only get the median of the variance-covariance matrix
#' vcov(x, conf_bands = 0.5)
#' }
coef.bvar <- function(
  object, type = c("quantile", "mean"), conf_bands = 0.5,
  companion = FALSE, ...) {

  type <- match.arg(type)

  if(companion) {return(companion.bvar(object, type, conf_bands, ...))}

  if(type == "quantile") {
    quantiles <- quantile_check(conf_bands)
    coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)
  } else {
    quantiles <- 0.5
    coefs <- apply(object[["beta"]], c(2, 3), mean)
  }

  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]
  vars <- name_deps(object[["variables"]], M = M)
  vars_expl <- name_expl(vars, M = M, lags = lags)

  if(length(quantiles) == 1) {
    dimnames(coefs)[[2]] <- vars
    dimnames(coefs)[[1]] <- vars_expl
  } else {
    dimnames(coefs)[[3]] <- vars
    dimnames(coefs)[[2]] <- vars_expl
  }

  class(coefs) <- append("bvar_coefs", class(coefs))

  return(coefs)
}


#' @rdname coef.bvar
#' @export
vcov.bvar <- function(
  object, type = c("quantile", "mean"), conf_bands = 0.5, ...) {

  type <- match.arg(type)

  if(type == "quantile") {
    quantiles <- quantile_check(conf_bands)
    vcovs <- apply(object[["sigma"]], c(2, 3), quantile, quantiles)
  } else {
    quantiles <- 0.5
    vcovs <- apply(object[["sigma"]], c(2, 3), mean)
  }

  vars <- name_deps(object[["variables"]], M = object[["meta"]][["M"]])

  if(length(quantiles) == 1) {
    dimnames(vcovs)[[1]] <- dimnames(vcovs)[[2]] <- vars
  } else {
    dimnames(vcovs)[[2]] <- dimnames(vcovs)[[3]] <- vars
  }

  class(vcovs) <- append("bvar_vcovs", class(vcovs))

  return(vcovs)
}


#' @export
print.bvar_coefs <- function(x, digits = 3L, complete = FALSE, ...) {

  .print_coefs(x, digits, type = "coefficient", complete = complete, ...)

  return(invisible(x))
}


#' @export
print.bvar_vcovs <- function(x, digits = 3L, complete = FALSE, ...) {

  if(!inherits(x, "bvar_vcovs")) {stop("Please provide a `bvar_vcovs` object.")}

  .print_coefs(x, digits, type = "variance-covariance", complete = complete, ...)

  return(invisible(x))
}


#' Coefficient and variance-covariance print method
#'
#' @param x Numeric array with coefficient or variance-covariance values of a
#' \code{bvar} object.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param type String indicating whether \emph{x} contains coefficient,
#' variance-covariance or forecast-error-variance decomposition values.
#' @param complete Logical scalar. Whether to print every contained quantile.
#'
#' @noRd
.print_coefs <- function(
  x, digits = 3L,
  type = c("coefficient", "variance-covariance", "FEVD", "companion"),
  complete = FALSE,
  ...) {

  type <- match.arg(type)

  has_quants <- length(dim(x)) == 3
  if(has_quants) {
    P <- dim(x)[1]
    coefs <- x["50%", , ]
    bands <- dimnames(x)[[1]]
  } else {coefs <- x[]} # Remove class to avoid recursion

  cat("Numeric array (dimensions ", paste0(dim(x), collapse = ", "),  ")",
    " of ", type, " values from a BVAR.\n", sep = "")
  if(has_quants) {
    cat("Computed confidence bands: ",
      paste(bands, collapse = ", "), "\n", sep = "")
  }
  if(complete && has_quants) {
    cat("Values:\n")
    for(band in bands) {
      cat("    ", band, ":\n", sep = "")
      print(round(x[band, , ], digits = digits))
    }
  } else {
    cat("Median values:\n")
    print(round(coefs, digits = digits))
  }

  return(invisible(x))
}
