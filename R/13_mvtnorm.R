#' @noRd
rmvn_proposal <- function(n, mean, sigma) {

  # Univariate cornercase
  if(length(sigma[["values"]]) == 1) {
    return(matrix(rnorm(n, mean = mean, sd = sigma[["values"]])))
  }

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
rmvn_inv <- function(n, sigma_inv, method = c("eigen", "chol")) {

  # We know sigma is positive definite, want its inverse and the mean is 0

  if(method == "eigen") {
    # Spectral  ---
    sigma_inv <- eigen(sigma_inv, symmetric = TRUE)
    m <- length(sigma_inv[["values"]])
    R <- t(sigma_inv[["vectors"]] %*%
      (t(sigma_inv[["vectors"]]) * sqrt(1 / pmax(sigma_inv[["values"]], 0))))
    out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R
  } else if(method == "chol") {
    # Cholesky ---
    m <- ncol(sigma_inv)
    R <- chol(sigma_inv)
    out <- t(backsolve(R, matrix(rnorm(n * m), ncol = n, byrow = TRUE)))
  }

  return(out)
}
