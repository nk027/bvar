bv_ml <- function(
  hyper = NULL, hyper_min = NULL, hyper_max = NULL,
  pars,
  priors,
  Y, X, K, M, N, lags) {


  # Bounds ------------------------------------------------------------------

  if(any(hyper_min > hyper | hyper > hyper_max)) return(list("log_ml" = -1e18))


  # Priors ------------------------------------------------------------------

  # Overwrite passed parameters with hyperparameters
  if(!is.null(hyper))
    for(name in unique(names(hyper)))
      pars[names(pars) == name] <- hyper[names(hyper) == name]

  psi <- diag(pars[names(pars) == "psi"])
  omega <- vector("numeric", 1 + M * lags)
  omega[1] <- priors[["var"]]
  for(i in 1:lags)
    omega[seq(2 + M * (i - 1), 1 + i * M)] <-
      pars[["lambda"]] ^ 2 / i ^ pars[["alpha"]] / pars[names(pars) == "psi"]

  # Dummy priors
  if(length(priors[["dummy"]]) > 0) {
    dmy <- lapply(priors[["dummy"]], function(x) {
      priors[[x]][["fun"]](Y = Y, lags = lags, pars = pars[[x]])
    })
    Y <- rbind(do.call(rbind, lapply(dmy, function(x)
      matrix(x[["Y"]], ncol = M))), Y)
    X <- rbind(do.call(rbind, lapply(dmy, function(x)
      matrix(x[["X"]], ncol = K))), X)
    N <- nrow(Y)
  }


  # Calc --------------------------------------------------------------------

  omega_inv <- diag(1 / omega)
  XX <- crossprod(X)
  beta_hat <- solve(XX + omega_inv) %*% (crossprod(X, Y) +
    omega_inv %*% priors[["b"]])
  sse <- crossprod(Y - X %*% beta_hat)
  psi_inv <- solve(sqrt(psi))
  omega_ml <- diag(sqrt(omega)) %*% XX %*% diag(sqrt(omega))
  psi_ml <- psi_inv %*%
    (sse + t(beta_hat - priors[["b"]]) %*% omega_inv %*%
      (beta_hat - priors[["b"]])) %*%
    psi_inv

  # Eigenvalues + 1 as another way of computing the determinants
  omega_ml_ev <- Re(eigen(omega_ml, only.values = TRUE)[["values"]])
  omega_ml_ev[omega_ml_ev < 1e-12] <- 0
  omega_ml_ev <- omega_ml_ev + 1
  psi_ml_ev <- Re(eigen(psi_ml, only.values = TRUE)[["values"]])
  psi_ml_ev[psi_ml_ev < 1e-12] <- 0
  psi_ml_ev <- psi_ml_ev + 1

  # Likelihood
  log_ml <- (-M * N * log(pi) / 2) +
    sum(lgamma(((N + M + 2) - 0:(M - 1)) / 2) -
      lgamma(((M + 2) - 0:(M -1)) / 2)) -
    (N * sum(log(diag(psi))) / 2) - (M * sum(log(omega_ml_ev)) / 2) -
    ((N + M + 2) * sum(log(psi_ml_ev)) / 2)

  # Add prior-pdfs
  if(length(priors[["dummy"]]) > 0) {
    log_ml <- log_ml + sum(sapply(priors[["dummy"]], function(x) {
      log(dgamma(pars[[x]],
                 shape = priors[[x]][["coef"]][["k"]],
                 scale = priors[[x]][["coef"]][["theta"]]))
      }))
  }


  # Output ------------------------------------------------------------------

  # Return log_ml and objects necessary for drawing
  out <- list("log_ml" = log_ml, "Y" = Y, "X" = X, "N" = N, "psi" = psi,
              "sse" = sse, "beta_hat" = beta_hat, "omega_inv" = omega_inv)

  return(out)
}
