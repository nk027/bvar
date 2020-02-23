#' @noRd
rmvn_proposal <- function(n, mean, sigma) {

  # Univariate cornercase
  if(length(mean) == 1) {
    return(matrix(rnorm(n, mean = mean, sd = sigma[["values"]])))
  }

  # Note that atm sigma is always a pure diagonal matrix

  # We have sigma's eigen-decomposition and know it is positive definite
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
    (t(sigma[["vectors"]]) * sqrt(sigma[["values"]])))
  out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R
  out <- sweep(out, 2, mean, "+")
  colnames(out) <- names(mean)

  return(out)
}


#' @noRd
rmvn_eta <- function(n, sigma_inv) {

  # We have sigma's eigen-decomposition, want its inverse and the mean is 0
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
    (t(sigma[["vectors"]]) * sqrt(1 / abs(sigma[["values"]]))))
  out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R

  return(out)
}


#' @noRd
rmvn_inv <- function(n, sigma_inv) {

  # We know sigma is positive definite, want its inverse and the mean is 0

  # Spectral  ---
  # sigma <- eigen(sigma, symmetric = TRUE)
  # m <- length(sigma[["values"]])
  # R <- t(sigma[["vectors"]] %*%
  #   (t(sigma[["vectors"]]) * sqrt(1 / pmax(sigma[["values"]], 0))))
  # out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R

  # Cholesky ---
  m <- ncol(sigma)
  R <- chol(sigma)
  out <- t(backsolve(R, matrix(rnorm(n * m), ncol = n, byrow = TRUE)))

  return(out)
}
