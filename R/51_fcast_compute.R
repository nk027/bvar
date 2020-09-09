
#' Forecast computation
#'
#' Compute unconditional forecasts without shocks from the VAR's posterior draws
#' obtained via \code{\link{draw_post}}.
#'
#' @param Y Numeric matrix (\eqn{N * M}).
#' @param K Integer scalar. Columns of \emph{X}, i.e. \eqn{M * lags + 1}.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param N Integer scalar. Rows of \emph{Y}, alternatively \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients of
#' the model in state space representation.
#' @param beta_const Numeric vector. Posterior draw of the VAR coefficients
#' corresponding to the constant of the model.
#'
#' @return Returns a matrix containing forecasts (without shocks) for all
#' variables in the model.
#'
#' @importFrom stats rnorm
#'
#' @noRd
compute_fcast <- function(
  Y, K, M, N, lags,
  horizon,
  beta_comp, beta_const) {

  Y_f <- matrix(NA, horizon + 1, K - 1)
  Y_f[1, ] <- vapply(t(Y[N:(N - lags + 1), ]), c, numeric(1L))

  for(i in seq.int(2, 1 + horizon)) {
    Y_f[i, ] <- tcrossprod(Y_f[i - 1, ], beta_comp) +
      c(beta_const, rep(0, M * (lags - 1))) # Maybe go back to normal beta
  }

  # Remove Y_t and lagged variables
  return(Y_f[-1, 1:M])
}
