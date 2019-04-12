#' BVAR posterior draws
#'
#' Draw beta and sigma from the posterior of a BVAR.
#'
#' @param X Numeric matrix. Possibly extended with dummy priors.
#' @param N Integer scalar. Rows of \emph{X}.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param b
#' @param psi
#' @param sse
#' @param beta_hat
#' @param omega_inv
#'
#' @return
#' @export
#'
#' @examples
draw_post <- function(
  X,
  N = nrow(X), M = ncol(X), lags, b,
  psi, sse, beta_hat, omega_inv) {

  S_post <- psi + sse + t(beta_hat - b) %*% omega_inv %*% (beta_hat - b)
  S_eig <- eigen(S_post)
  S_inv <- S_eig[["vectors"]] %*%
    diag(1 / abs(S_eig[["values"]])) %*% t(S_eig[["vectors"]])

  eta <- MASS::mvrnorm(n = (N + M + 2), mu = rep(0, M), Sigma = S_inv)
  sigma_draw <- solve(crossprod(eta)) %*% diag(M)
  sigma_chol <- t(chol(sigma_draw))
  beta_draw <- beta_hat +
    t(MASS::mvrnorm(n = M, mu = rep(0, (1 + M * lags)),
      Sigma = solve(crossprod(X) + omega_inv) %*% diag(1 + M * lags))) %*%
    sigma_chol

  return(list("beta_draw" = beta_draw,
              "sigma_draw" = sigma_draw,
              "sigma_chol" = sigma_chol))
}
