bv_fcast <- function(Y,
  Y_row, Y_col, X_col, lags, horizon,
  beta, constant, sigma, # Maybe leave the constant in beta
  fcast_exo = NULL) {

  Y_f <- matrix(NA, horizon + 1, X_col - 1)
  Y_f[1, ] <- sapply(t(Y[Y_row:(Y_row - lags + 1), ]), c)

  if(is.null(fcast_exo)) {
    # Unconditional forecast
    for(i in 2:(1 + horizon)) {
      Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta) +
        c(const, rep(0, Y_col + (lags - 1))) +
        c(sigma %*% rnorm(Y_col), rep(0, Y_col * (lags - 1)))
    }
  } else {
    # Conditional forecast
    Y_f[2:(fcast_exo$hor + 1), fcast_exo$pos] <- fcast_exo$path
    for(i in 2:(1 + horizon)) { # same as unconditional
      Y_temp <- Y_f[i - 1, ] %*% t(beta) +
        c(const, rep(0, Y_col * (lags - 1))) +
        c(sigma %*% rnorm(Y_col), rep(0, Y_col * (lags - 1)))
      if(i < (fcast_exo$hor + 2)) { # do no replace the exogenous path
        Y_f[i, -fcast_exo$pos] <- Y_temp[-fcast_exo$pos]
      } else {
        Y_f[i, ] <- Y_temp
      }
    }
  }

  return(Y_f[2:(1 + horizon), 1:Y_col])
}
