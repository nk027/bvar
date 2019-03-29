int_check <- function(
  x, min = 0, max = Inf,
  msg = "Please check the integer parameters.") {

  if(!is.numeric(x) || length(x) != 1 || x < min || x > max) {
    stop(msg)
  } else {
    return(as.integer(x))
  }
}

auto_psi <- function(Y, lags) {

  x <- list()
  x[["mode"]] <- tryCatch(apply(Y, 2, function(x) {
    sqrt(arima(x, order = c(lags, 0, 0))$sigma2)
  }), error = function(x) {
    # Could increase the order of integration here - not done for transparency
    stop("Data appears to be integrated. ",
         "Setting psi automatically via 'arima()' (p = ", lags, ") failed.")
  })

  x[["min"]] <- x[["mode"]] / 100
  x[["max"]] <- x[["mode"]] * 100

  return(x)
}
