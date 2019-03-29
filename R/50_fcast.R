bv_fcast <- function(
  horizon = 12,
  exogenous = NULL) {

  horizon = int_check(horizon, min = 1, max = 1e6,
                      msg = "Invalid value for horizon (outside of [1, 1e6]).")

  out <- list("horizon" = horizon, "exogenous" = exogenous)
  class(out) <- "bv_fcast"

  return(out)
}


compute_fcast <- function(
  Y, K, M, N, lags,
  horizon,
  beta_comp, constant,
  sigma,
  exogenous = NULL) {

  Y_f <- matrix(NA, horizon + 1, K - 1)
  Y_f[1, ] <- sapply(t(Y[N:(N - lags + 1), ]), c)

  for(i in 2:(1 + horizon)) {
    Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta) +
      c(const, rep(0, M + (lags - 1))) +
      c(sigma %*% rnorm(M), rep(0, M * (lags - 1)))
  }

  # Removed exogenous forecasts for now
  # # if(is.null(exogenous)) {
  #   # Unconditional forecast
  #   for(i in 2:(1 + horizon)) {
  #     Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta) +
  #       c(const, rep(0, M + (lags - 1))) +
  #       c(sigma %*% rnorm(M), rep(0, M * (lags - 1)))
  #   }
  # } else {
  #   # Conditional forecast
  #   Y_f[2:(exogenous$hor + 1), exogenous$pos] <- exogenous$path
  #   for(i in 2:(1 + horizon)) { # same as unconditional
  #     Y_temp <- Y_f[i - 1, ] %*% t(beta) +
  #       c(const, rep(0, M * (lags - 1))) +
  #       c(sigma %*% rnorm(M), rep(0, M * (lags - 1)))
  #     if(i < (exogenous$hor + 2)) { # do no replace the exogenous path
  #       Y_f[i, -exogenous$pos] <- Y_temp[-exogenous$pos]
  #     } else {
  #       Y_f[i, ] <- Y_temp
  #     }
  #   }
  # }

  # Remove Y_t and further lags from Y_f to get the forecast
  out <- Y_f[2:(1 + horizon), 1:M]

  return(out)
}
