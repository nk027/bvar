#' Coefficient and VCOV methods for Bayesian VARs
#'
#' Retrieves coefficient / VCOV values for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#'
#' @param x Object of class \code{bvar_coefs} / \code{bvar_vcovs}.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied
#' to numeric outputs (i.e. the quantiles).
#' @param ... Not used.
#'
#' @return Returns a numeric array of class \code{bvar_coefs} /
#' \code{bvar_vcovs} with desired values at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
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
coef.bvar <- function(object, conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  quantiles <- quantile_check(conf_bands)
  coefs <- apply(object[["beta"]], c(2, 3), quantile, quantiles)
  class(coefs) <- "bvar_coefs"

  return(coefs)
}


#' @rdname coef.bvar
#' @export
vcov.bvar <- function(object, conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  quantiles <- quantile_check(conf_bands)
  vcovs <- apply(object[["sigma"]], c(2, 3), quantile, quantiles)
  class(vcovs) <- "bvar_vcovs"

  return(vcovs)
}


#' @rdname coef.bvar
#' @export
print.bvar_coefs <- function(x, digits = 3L, ...) {

  if(!inherits(x, "bvar_coefs")) {stop("Please provide a `bvar_coefs` object.")}

  print_coefs(x, digits, type = "coefficient", ...)

  return(invisible(x))
}


#' @rdname coef.bvar
#' @export
print.bvar_vcovs <- function(x, digits = 3L, ...) {

  if(!inherits(x, "bvar_vcovs")) {stop("Please provide a `bvar_vcovs` object.")}

  print_coefs(x, digits, type = "variance-covariance", ...)

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
  type = c("coefficient", "variance-covariance",
           "forecast error variance decomposition"),
  ...) {

  type <- match.arg(type)

  has_quants <- length(dim(x)) == 3
  if(has_quants) {
    P <- dim(x)[1]
    coefs <- x[, , ]
  } else {coefs <- x[]} # Remove class, avoid recursion

  cat(gsub("^(.)(.*)", "\\U\\1\\L\\2", type, perl = TRUE),
      "values of a Bayesian VAR.\n")
  if(has_quants) {
    cat("Computed confidence bands: ",
        paste(dimnames(x)[[1]], collapse = ", "), "\n", sep = "")
    for(j in dimnames(x)[[1]]) {
      if(j == "50%") {
        cat("Median values:\n")
        print(round(coefs[j, , ], digits = digits))
      } else {
        cat(j, ":\n")
        print(round(coefs[j, , ], digits = digits))
      }
    }

  } else {
    cat("Median values:\n")
    print(round(coefs, digits = digits))
  }

  return(invisible(x))
}
