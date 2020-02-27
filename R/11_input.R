#' Check numeric scalar
#'
#' Check whether an object is bounded and coercible to a numeric value.
#'
#' @param x Numeric scalar.
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

  if(!is.numeric(x) || length(x) != 1 || x < min || x > max) {stop(msg)}

  return(fun(x))
}


#' @noRd
int_check <- function(
  x, min = 0L, max = Inf,
  msg = "Please check the integer parameters.") {

  num_check(x, min, max, msg, fun = as.integer)
}


#' Auto-set psi of the Minnesota prior
#'
#' Automatically set the prior values of \emph{psi}. Fits an \eqn{AR(p)} model
#' and sets the mode to the square-root of the innovations variance. Boundaries
#' are set to the mode times / divided by 100.
#'
#' If the call to \code{\link[stats]{arima}} fails, an integrated
#' \eqn{ARIMA(p, 1, 0)} model is fitted instead.
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
    tryCatch(sqrt(arima(x, order = c(lags, 0, 0))$sigma2), # Try AR(lags)
      error = function(e) { # If this fails for a series, increment integration
        message("Some of the data appears to be integrated. Attempting to set ",
          "psi automatically via an ARIMA(", lags, ", 1, 0) model.")
        sqrt(arima(x, order = c(lags, 1, 0))$sigma2) # Try ARIMA(lags, 1, 0)
    })}), error = function(e) {
      stop("Some of the data appears to be integrated of order higher than 1. ",
        "Setting psi automatically failed. Please inspect the data again ",
        "and/or provide modes for psi manually (see `?bv_psi`).")
    }
  )

  out[["min"]] <- out[["mode"]] / 100
  out[["max"]] <- out[["mode"]] * 100

  return(out)
}
