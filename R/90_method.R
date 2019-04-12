plot.bvar <- function(x, ...) {

  hyper_plot(x, ...)

}


print.bvar <- function(x, ...) {

  cat("\nBayesian VAR consisting of", x$meta$N, "observations,",
      x$meta$M, "variables and", x$meta$lags, "lags.")
  cat("\nHyperparameters:", paste(x$priors$hyper, collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x$optim$par, 3), collapse = ", "))
  cat("\nIterations (burnt): ", x$meta$n_draw, " (", x$meta$n_burn, ")",
      sep = "")
  cat("\nAccepted draws (rate): ", x$accepted, " (",
      round(x$accepted / x$meta$n_draw, 3), ")", sep = "")
  # if(irf)
  #   if(fevd) if(M < x)
  # if(fcast)

  return(invisible(x))
}


print.bvar_irf <- function(x, ...) {

  cat("...")

}


print.bvar_fcast <- function(x, ...) {

  cat("...")

}
