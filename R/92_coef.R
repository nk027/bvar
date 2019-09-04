#' Coefficient and VCOV methods for Bayesian VARs
#'
#' Retrieves coefficient / VCOV values for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#' @param comp Logical scalar. Whether to retrieve the companion matrix of
#' coefficients. See \code{\link{companion.bvar}}.
#'
#' @param x Object of class \code{bvar_coefs} / \code{bvar_vcovs}.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied
#' to numeric outputs (i.e. the quantiles).
#' @param complete Logical scalar. Whether to print only medians or all
#' available confidence bands.
#' @param ... Not used.
#'
#' @return Returns a numeric array of class \code{bvar_coefs} /
#' \code{bvar_vcovs} with desired values at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @importFrom stats coef vcov
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get coefficent values at the 10%, 50% and 90% quantiles
#' coef(x, conf_bands = 0.10)
#'
#' # Only get the median of the variance-covariance matrix
#' vcov(x, conf_bands = 0.5)
#' }
coef.bvar <- function(
  object, conf_bands = 0.5,
  comp = FALSE, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  if(comp) {return(companion.bvar(object, conf_bands, ...))}

  quantiles <- quantile_check(conf_bands)
  coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)

  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]
  vars <- object[["variables"]]
  if(is.null(vars)) {vars <- paste0("var", 1:M)}

  vars_row <- c("const", paste0(rep(vars, lags), "-lag", rep(1:lags, each = M)))
  if(length(quantiles) == 1) {
    dimnames(coefs)[[2]] <- vars
    dimnames(coefs)[[1]] <- vars_row
  } else {
    dimnames(coefs)[[3]] <- vars
    dimnames(coefs)[[2]] <- vars_row
  }

  class(coefs) <- append("bvar_coefs", class(coefs))

  return(coefs)
}


#' @rdname coef.bvar
#' @export
vcov.bvar <- function(object, conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  quantiles <- quantile_check(conf_bands)
  vcovs <- apply(object[["sigma"]], c(2, 3), quantile, quantiles)

  vars <- object[["variables"]]
  if(is.null(vars)) {
    M <- object[["meta"]][["M"]]
    vars <- paste0("var", 1:M)
  }

  if(length(quantiles) == 1) {
    dimnames(vcovs)[[1]] <- dimnames(vcovs)[[2]] <- vars
  } else {
    dimnames(vcovs)[[2]] <- dimnames(vcovs)[[3]] <- vars
  }

  class(vcovs) <- append("bvar_vcovs", class(vcovs))

  return(vcovs)
}


#' @rdname coef.bvar
#' @export
print.bvar_coefs <- function(x, digits = 3L, complete = FALSE, ...) {

  if(!inherits(x, "bvar_coefs")) {stop("Please provide a `bvar_coefs` object.")}

  print_coefs(x, digits, type = "coefficient", complete = complete, ...)

  return(invisible(x))
}


#' @rdname coef.bvar
#' @export
print.bvar_vcovs <- function(x, digits = 3L, complete = FALSE, ...) {

  if(!inherits(x, "bvar_vcovs")) {stop("Please provide a `bvar_vcovs` object.")}

  print_coefs(x, digits, type = "variance-covariance", complete = complete, ...)

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
#'
#' @noRd
print_coefs <- function(
  x, digits = 3L,
  type = c("coefficient", "variance-covariance", "FEVD"),
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
