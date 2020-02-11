#' BVAR posterior draws
#'
#' Draw \eqn{\beta} and \eqn{\sigma} from the posterior of a Bayesian VAR.
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
#' @return Returns a list with the following elements:
#' \itemize{
#'   \item \code{beta_draw}, \code{sigma_draw} - Draws from the posterior.
#'   \item \code{sigma_chol} - The lower part of a Cholesky decomposition
#'   of sigma_draw. Calculated as \code{t(chol(sigma_draw))}.
#' }
#'
#' @importFrom mvtnorm rmvnorm
#'
#' @noRd
draw_post <- function(
  XX, N, M, lags,
  b, psi, sse, beta_hat, omega_inv) {

  S_post <- psi + sse + t(beta_hat - b) %*% omega_inv %*% (beta_hat - b)
  S_eig <- eigen(S_post)
  S_inv <- S_eig[["vectors"]] %*%
    diag(1 / abs(S_eig[["values"]])) %*% t(S_eig[["vectors"]])

  eta <- rmvnorm(n = (N + M + 2), mean = rep(0, M), sigma = S_inv)
  sigma_draw <- invert(eta, crossprod = TRUE)
  # sigma_draw <- solve(crossprod(eta))
  sigma_chol <- t(chol(sigma_draw))
  beta_sigma <- invert(XX + omega_inv)
  beta_draw <- beta_hat +
    t(rmvnorm(n = M, mean = rep(0, (1 + M * lags)), sigma = beta_sigma)) %*%
    sigma_chol

  return(list("beta_draw" = beta_draw,
              "sigma_draw" = sigma_draw,
              "sigma_chol" = sigma_chol))
}


#' Factorised inverse
#'
#' Invert a matrix via a factorisation. Helper function to create
#' inverses for the crossproduct of \emph{eta} and the sum of \emph{XX} and
#' \emph{omega_inv}. Attempts inversion via singular value decomposition and,
#' subsequently via the Cholesky decomposition.
#'
#' @param A Numeric matrix to invert.
#' @param crossprod Logical scalar. Whether to return \eqn{A^{-1}} or
#' \eqn{(A'A)^{-1}}.
#'
#' @return Returns either \eqn{A^{-1}} or \eqn{(A'A)^{-1}}.
#'
#' @noRd
invert <- function(A, crossprod = FALSE) {
  if(crossprod) {
    return(tryCatch({
      svd <- svd(A)
      sigma_draw <- tcrossprod(svd$v %*% diag(1 / svd$d))
    }, error = function(e) {tryCatch({
      warning("Error inverting via `svd()`, trying `chol2inv()`.")
      chol2inv(chol(crossprod(A)))
    }, error = function(e) {stop(e)})}))
  } else {
    return(tryCatch({
      svd <- svd(A)
      sigma_draw <- svd$v %*% diag(1 / svd$d) %*% t(svd$u)
    }, error = function(e) {tryCatch({
      warning("Error inverting via `svd()`, trying `chol2inv()`.")
      chol2inv(chol(A))
    }, error = function(e) {stop(e)})}))
  }
}
