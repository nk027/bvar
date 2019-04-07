compute_fcast <- function(
  Y, K, M, N, lags,
  horizon,
  beta_comp, beta_const,
  sigma,
  conditional = NULL) {

  Y_f <- matrix(NA, horizon + 1, K - 1)
  Y_f[1, ] <- sapply(t(Y[N:(N - lags + 1), ]), c)

  for(i in 2:(1 + horizon)) {
    Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta_comp) +
      c(beta_const, rep(0, M * (lags - 1))) +
      c(sigma %*% rnorm(M), rep(0, M * (lags - 1)))
  }

  # Remove Y_t and further lags from Y_f to get the forecast
  return(Y_f[2:(1 + horizon), 1:M])
}
