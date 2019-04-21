#' Forecast draws
#'
#' Computes unconditional forecasts using the posterior draws of the VAR
#' coefficients and vcov-matrix obtained from \code{\link{draw_post}}.
#'
#'
#' @param Y Numeric \eqn{N * M} matrix.
#' @param K Integer scalar. Columns of X, i.e. \eqn{M * lags + 1}.
#' @param M Integer scalar. Columns of Y.
#' @param N Integer scalar. Rows of Y, alternatively X.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients of
#' the model in state space representation.
#' @param beta_const Numeric vector. Posterior draw of the VAR coefficients
#' corresponsing to the constant of the model.
#' @param sigma Numeric matrix. Posterior draw of the vcoc-matrix of the
#' model.
#' @param conditional List containing the conditioning path of certain
#' variables and their name or position in the dataset. Currently not
#' implemented, thus not functionable.
#'
#' @return Returns a matrix containing forecasts for all variables in the model.
compute_fcast <- function(
  Y, K, M, N, lags,
  horizon,
  beta_comp, beta_const,
  sigma,
  conditional = NULL) {

  Y_f <- matrix(NA, horizon + 1, K - 1)
  Y_f[1, ] <- sapply(t(Y[N:(N - lags + 1), ]), c)

  for(i in 2:(1 + horizon)) {
    Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta_comp) +
      c(beta_const, rep(0, M * (lags - 1))) +
      c(sigma %*% rnorm(M), rep(0, M * (lags - 1)))
  }

  # Remove Y_t and further lags from Y_f to get the forecast
  return(Y_f[2:(1 + horizon), 1:M])
}
