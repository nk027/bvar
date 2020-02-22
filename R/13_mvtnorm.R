#' @noRd
rmvn_proposal <- function(n, mean, sigma) {

  # Univariate cornercase
  if(length(mean) == 1) {return(matrix(rnorm(n, mean = mean, sd = sigma)))}

  # We have sigma's eigen-decomposition and know it is positive definite
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
    t(sigma[["vectors"]]) * sqrt(sigma[["values"]]))
  out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R
  out <- sweep(out, 2, mean, "+")

  return(out)
}


#' @noRd
rmvn_eta <- function(n, sigma) {

  # We have sigma's eigen-decomposition and the mean is 0
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
    t(sigma[["vectors"]]) * sqrt(1 / abs(sigma[["values"]])))
  out <- matrix(rnorm(n * m), nrow = n, byrow = TRUE) %*% R

  return(out)
}


#' @noRd
rmvn_noise <- function(n, sigma) {

  # We know sigma is symmetric and the mean is 0
  R <- chol(sigma, pivot = TRUE)
  R <- R[, order(attr(R, "pivot"))]
  out <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = TRUE) %*% R

  return(out)
}
