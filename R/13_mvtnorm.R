
#' Optimised multivariate normal drawing
#'
#' Function to quickly draw from a multivariate normal distribution in special
#' cases required by \code{\link{bvar}}. Based on the implementation by
#' Friedrich Leisch and Fabian Scheipl in \code{\link[mvtnorm]{rmvnorm}}.
#'
#' The two special cases are (1) the proposal, where the spectral decomposition
#' of \emph{sigma} only needs to be calculated once, and (2) drawing when only
#' the inverse of \emph{sigma} is available. Note that we skip steps to check
#' the symmetry and definiteness of \emph{sigma}, since these properties are
#' given per construction.
#'
#' @param n Numeric scalar. Number of draws.
#' @param mean Numeric vector. Mean of the draws.
#' @param sigma Numeric matrix. Variance-covariance of draws.
#' @param sigma_inv Numeric matrix. Inverse of variance-covariance of draws.
#' @param method Character scalar. Type of decomposition to use.
#'
#' @return Returns a numeric matrix of draws.
#'
#' @noRd
rmvn_proposal <- function(n, mean, sigma) {

  # Univariate cornercase ---
  if(length(sigma[["values"]]) == 1) {
    out <- matrix(rnorm(n, mean = mean, sd = sigma[["values"]]))
    colnames(out) <- names(mean)
    return(out)
  }
  # Multivariate ---
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
    (t(sigma[["vectors"]]) * sqrt(sigma[["values"]])))
  out <- matrix(rnorm(n * m), nrow = n, ncol = m, byrow = TRUE) %*% R
  out <- sweep(out, 2, mean, "+")
  colnames(out) <- names(mean)

  return(out)
}


#' @noRd
rmvn_inv <- function(n, sigma_inv, method) {

  if(method == "eigen") {
    # Spectral  ---
    sigma_inv <- eigen(sigma_inv, symmetric = TRUE)
    m <- length(sigma_inv[["values"]])
    R <- t(sigma_inv[["vectors"]] %*%
      (t(sigma_inv[["vectors"]]) * sqrt(1 / pmax(sigma_inv[["values"]], 0))))
    out <- matrix(rnorm(n * m), nrow = n, ncol = m, byrow = TRUE) %*% R
  } else if(method == "chol") {
    # Cholesky ---
    m <- ncol(sigma_inv)
    R <- chol(sigma_inv)
    out <- t(backsolve(R,
      matrix(rnorm(n * m), nrow = m, ncol = n, byrow = TRUE)))
  } else {stop("SOMEBODY TOUCHA MY SPAGHET!")}

  return(out)
}
