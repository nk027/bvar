
#' BVAR posterior draws
#'
#' Draw \eqn{\beta} and \eqn{\sigma} from the posterior.
#'
#' @param XX Numeric matrix. Crossproduct of a possibly extended \emph{X}.
#' @param N Integer scalar. Rows of \emph{X}. Note that \emph{X} may have been
#' extended with dummy observations.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param b Numeric marix. Minnesota prior's mean.
#' @param psi Numeric matrix. Scale of the IW prior on the residual covariance.
#' @param sse Numeric matrix. Squared VAR residuals.
#' @param beta_hat Numeric matrix.
#' @param omega_inv Numeric matrix.
#'
#' @return Returns a list with the following posterior draws of \emph{beta} and
#' \emph{sigma}.
#'
#' @importFrom mvtnorm rmvnorm
#'
#' @noRd
draw_post <- function(
  XX, N, M, lags,
  b, psi, sse, beta_hat, omega_inv) {

  S_post <- psi + sse + crossprod((beta_hat - b), omega_inv) %*% (beta_hat - b)
  eta <- rmvn_inv(n = (N + M + 2), sigma_inv = S_post, method = "eigen")

  chol_de <- chol(crossprod(eta))
  sigma_chol <- forwardsolve(t(chol_de), diag(nrow(chol_de)))
  sigma_draw <- crossprod(sigma_chol)

  noise <- rmvn_inv(n = M, sigma_inv = XX + omega_inv, method = "chol")

  beta_draw <- beta_hat + crossprod(noise, sigma_chol)

  return(list("beta_draw" = beta_draw, "sigma_draw" = sigma_draw))
}
