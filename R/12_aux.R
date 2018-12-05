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
