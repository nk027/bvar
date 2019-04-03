var_lag <- function(x, p) {

  x_rows <- nrow(x)
  x_cols <- ncol(x)

  x_lagged <- matrix(0, x_rows, p * x_cols)
  for(i in 1:p) {
    x_lagged[(p + 1):x_rows, (x_cols * (i - 1) + 1):(x_cols * i)] <-
      x[(p + 1 - i):(x_rows - i), (1:x_cols)]
  }

  return(x_lagged)
}


gamma_coef <- function(mode, sd) {

  mode_sq <- mode ^ 2
  sd_sq <- sd ^ 2
  k <- (2 + mode_sq / sd_sq + sqrt((4 + mode_sq / sd_sq) * mode_sq / sd_sq)) / 2
  theta <- sqrt(sd_sq / k)

  return(list("k" = k, "theta" = theta))
}


name_pars <- function(x, M) {

  Reduce(c, sapply(x, function(y) if(y == "psi") rep(y, M) else y))
}
