
#' Lag a matrix
#'
#' Compute a lagged version of a matrix to be used in vector autoregressions.
#' Higher lags are further to the right.
#'
#' @param x Matrix (\eqn{N * M}) to lag.
#' @param lags Integer scalar. Number of lags to apply.
#'
#' @return Returns an \eqn{N * (M * lags)} matrix with consecutive lags on the
#' right. The elements of the first \emph{lags} rows are 0.
#'
#' @noRd
lag_var <- function(x, lags) {

  x_rows <- nrow(x)
  x_cols <- ncol(x)

  x_lagged <- matrix(0, x_rows, lags * x_cols)
  for(i in 1:lags) {
    x_lagged[(lags + 1):x_rows, (x_cols * (i - 1) + 1):(x_cols * i)] <-
      x[(lags + 1 - i):(x_rows - i), (1:x_cols)]
  }

  return(x_lagged)
}


#' Compute gamma coefficients
#'
#' Compute the shape \emph{k} and scale \emph{theta} of a Gamma
#' distribution via the mode and standard deviation.
#'
#' @param mode Numeric scalar.
#' @param sd Numeric scalar.
#'
#' @return Returns a list with shape \emph{k} and scale parameter \emph{theta}.
#'
#' @noRd
gamma_coef <- function(mode, sd) {

  mode_sq <- mode ^ 2
  sd_sq <- sd ^ 2
  k <- (2 + mode_sq / sd_sq + sqrt((4 + mode_sq / sd_sq) * mode_sq / sd_sq)) / 2
  theta <- sqrt(sd_sq / k)

  return(list("k" = k, "theta" = theta))
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


#' Compute companion matrix
#'
#' Compute the companion form of the VAR coefficients.
#'
#' @param beta Numeric (\eqn{K * M}) matrix with VAR coefficients.
#' @param K Integer scalar. Number of columns in the independent data.
#' @param M Integer scalar. Number of columns in the dependent data.
#' @param lags Integer scalar. Number of lags applied.
#'
#' @return Returns a numeric (\eqn{K - 1 * K -1}) matrix with \emph{beta} in
#' companion form.
#'
#' @noRd
get_beta_comp <- function(beta, K, M, lags) {

  beta_comp <- matrix(0, K - 1, K - 1)

  beta_comp[1:M, ] <- t(beta[2:K, ]) # Kick constant
  if(lags > 1) { # Add block-diagonal matrix beneath VAR coefficients
    beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
  }

  return(beta_comp)
}
