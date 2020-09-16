
#' Calculate the log marginal likelihood
#'
#' @noRd
calc_logml <- function(M, N, psi, omega_ml_ev, psi_ml_ev) {

  return(
    (-M * N * log(pi) / 2) + sum(lgamma(((N + M + 2) - seq.int(0, M - 1)) / 2) -
      lgamma(((M + 2) - seq.int(0, M -1)) / 2)) -
    (N * sum(log(diag(psi))) / 2) - (M * sum(log(omega_ml_ev)) / 2) -
    ((N + M + 2) * sum(log(psi_ml_ev)) / 2)
  )
}


#' Calculate eigenvalues to bypass determinant computation
#'
#' @noRd
calc_ev <- function(
  omega_inv, omega_sqrt, psi_inv,
  X, XX, Y, b, beta_hat = TRUE) {

  if(is.null(XX)) {XX <- crossprod(X)}

  beta_hat <- if(beta_hat) { # Could factorise
    solve(XX + omega_inv, crossprod(X, Y) + omega_inv %*% b)
  } else {b}

  sse <- crossprod(Y - X %*% beta_hat)
  omega_ml <- omega_sqrt %*% XX %*% omega_sqrt
  mostly_harmless <- sse + if(all(beta_hat == b)) {0} else {
    crossprod(beta_hat - b, omega_inv) %*% (beta_hat - b)
  }
  psi_ml <- psi_inv %*% mostly_harmless %*% psi_inv

  # Eigenvalues + 1 as another way of computing the determinants
  omega_ml_ev <- Re(eigen(omega_ml,
    symmetric = TRUE, only.values = TRUE)[["values"]])
  omega_ml_ev[omega_ml_ev < 1e-12] <- 0
  omega_ml_ev <- omega_ml_ev + 1

  psi_ml_ev <- Re(eigen(psi_ml,
    symmetric = TRUE, only.values = TRUE)[["values"]])
  psi_ml_ev[psi_ml_ev < 1e-12] <- 0
  psi_ml_ev <- psi_ml_ev + 1

  return(
    list("omega" = omega_ml_ev, "psi" = psi_ml_ev,
      "sse" = sse, "beta_hat" = beta_hat)
  )
}
