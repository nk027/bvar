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

  return(invisible(x))
}

summary.bvar <- function(x, ...) {

  0
  class(out) <- "summary.bvar"

}

print.summary.bvar <- function(x, ...) {

  0

}


plot.bvar_irf <- function(x, ...) {

  bv_plot_irf(...)

}


print.bvar_irf <- function(x, ...) {

  cat("...")

}


plot.bvar_fcast <- function(x, ...) {

  bv_plot_irf(...)

}


print.bvar_fcast <- function(x, ...) {

  cat("...")

}
