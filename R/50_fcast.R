bv_fcast <- function(
  horizon = 12,
  conditional = NULL) {

  horizon = int_check(horizon, min = 1, max = 1e6,
                      msg = "Invalid value for horizon (outside of [1, 1e6]).")

  out <- list("horizon" = horizon, "conditional" = conditional)
  class(out) <- "bv_fcast"

  return(out)
}


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
  out <- Y_f[2:(1 + horizon), 1:M]

  return(out)
}
