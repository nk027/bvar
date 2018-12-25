.int_check <- function(x, min = 0, max = Inf,
                       msg = "Please check the integer parameters.") {
  if(!is.numeric(x) || length(x) != 1 || x < min || x > max) {
    stop(msg)
  } else {
    return(as.integer(x))
  }
}

.auto_psi <- function(Y, lags) {
  x <- list()
  x$mode <- tryCatch(apply(Y, 2, function(x) {
    sqrt(arima(x, order = c(lags, 0, 0))$sigma2)
  }), error = function(x) {
    stop("Data appears to be integrated; setting psi automatically via 'arima' failed.")
  })
  # # Alternative via try(), which is allegedly less efficient. Upping the
  # # order of integration is done away with for transparency (for now).
  # x <- try(apply(Y, 2, function(x) {
  #   sqrt(arima(x, order = c(lags, 0, 0))$sigma2)
  # }), silent = TRUE)
  # if(is(priors$psi$mode, "try-error")) {
  #   priors$psi$mode <- try(apply(Y, 2, function(x) {
  #     sqrt(arima(x, order = c(lags, 1, 0))$sigma2)
  #   }), silent = TRUE)
  #   if (is(priors$psi$mode, "try-error")) {
  #     stop("Order of integration exceeds 1. Please inspect data and try again.")
  #   }
  # }

  x$min <- x$mode / 100
  x$max <- x$mode * 100

  return(x)
}
