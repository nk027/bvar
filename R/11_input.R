#' Check a numeric scalar
#'
#' Function to check whether an object is properly bounded and coercible to a
#' a numeric value.
#'
#' @param x Numeric scalar to check.
#' @param min Numeric scalar. Minimum value of \emph{x}.
#' @param max Numeric scalar. Maximum value of \emph{x}.
#' @param fun Function to apply to \emph{x} before returning.
#' @param msg String fed to \code{\link[base]{stop}} if an error occurs.
#'
#' @return Returns \code{fun(x)}.
#'
#' @noRd
num_check <- function(
  x, min = 0, max = Inf,
  msg = "Please check the numeric parameters.",
  fun = as.numeric) {

  if(!is.numeric(x) || length(x) != 1 || x < min || x > max) {
    stop(msg)
  }

  return(fun(x))
}

#' @rdname num_check
#'
#' @noRd
int_check <- function(
  x, min = 0, max = Inf,
  msg = "Please check the integer parameters.") {

  num_check(x, min, max, msg, fun = as.integer)
}

#' Set psi of the Minnesota prior
#'
#' Set the prior values of \emph{psi} by fitting an \eqn{AR(p)} model and using
#' the squareroot of the innovations variance.
#'
#' @param x Numeric matrix with the data.
#' @param lags Numeric scalar. Number of lags in the model.
#'
#' @importFrom stats arima
#'
#' @return Returns a list with the modes, minimum, and maximum values for
#' \emph{psi}.
#'
#' @noRd
auto_psi <- function(x, lags) {

  out <- list()
  out[["mode"]] <- tryCatch(apply(x, 2, function(x) {
    sqrt(arima(x, order = c(lags, 0, 0))$sigma2)
  }), error = function() {
    stop("Data appears to be integrated. ",
         "Setting psi automatically via `arima()` (p = ", lags, ") failed.")
  })

  out[["min"]] <- out[["mode"]] / 100
  out[["max"]] <- out[["mode"]] * 100

  return(out)
}


#' Generate quantiles
#'
#' Check and create a given vector of confidence bands and create suitable
#' quantiles from it.
#'
#' @param conf_bands Numeric vector of probabilities (\eqn{[0, 1]}).
#'
#' @return Returns a sorted vector of quantiles created from \emph{conf_bands}.
#'
#' @examples
#' bvar:::quantile_check(c(0.1, 0.16))
#'
#' @noRd
quantile_check <- function(conf_bands) {

  if(any(!is.numeric(conf_bands), conf_bands > 1, conf_bands < 0)) {
    stop("Confidence bands misspecified.")
  }

  return(sort(c(conf_bands, 0.5, (1 - conf_bands))))
}
