plot.bvar <- function(x, ...) {
  hyper_plot(x)
  # if(any(names(x) == "irf")) {irf_plot(x, ...)}
  # if(any(names(x) == "fcast")) {fcast_plot(x, ...)}
}

summary.bvar <- function(x, ...) {

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

  return(invisible(x))
}
