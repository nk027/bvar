#' BVAR posterior draws
#'
#' Draw beta and sigma from the posterior of a Bayesian VAR.
#'
#' @param X Numeric matrix. Possibly extended with dummy priors.
#' @param N Integer scalar. Rows of \emph{X}.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param b Numeric marix. Minnesota prior mean.
#' @param psi Numeric matrix. Scale of the IW prior on the residual covariance.
#' @param sse Numeric matrix. Squared VAR residuals.
#' @param beta_hat Numeric matrix.
#' @param omega_inv Numeric matrix.
#'
#' @return Returns a list with the following elements:
#' \itemize{
#'   \item \code{beta_draw}, \code{sigma_draw} - Draws from the posterior.
#'   \item \code{sigma_chol} - The lower part of a Cholesky decomposition
#'   of sigma_draw. Calculated as \code{t(chol(sigma_draw))}.
#' }
#'
#' @importFrom MASS mvrnorm
#'
#' @noRd
draw_post <- function(
  X,
  N = nrow(X), M = ncol(X), lags, b,
  psi, sse, beta_hat, omega_inv) {

  S_post <- psi + sse + t(beta_hat - b) %*% omega_inv %*% (beta_hat - b)
  S_eig <- eigen(S_post)
  S_inv <- S_eig[["vectors"]] %*%
    diag(1 / abs(S_eig[["values"]])) %*% t(S_eig[["vectors"]])

  eta <- mvrnorm(n = (N + M + 2), mu = rep(0, M), Sigma = S_inv)
  sigma_draw <- solve(crossprod(eta)) %*% diag(M)
  sigma_chol <- t(chol(sigma_draw))
  beta_draw <- beta_hat +
    t(mvrnorm(n = M, mu = rep(0, (1 + M * lags)),
      Sigma = solve(crossprod(X) + omega_inv) %*% diag(1 + M * lags))) %*%
    sigma_chol

  return(list("beta_draw" = beta_draw,
              "sigma_draw" = sigma_draw,
              "sigma_chol" = sigma_chol))
}
