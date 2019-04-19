print.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  cat("Bayesian VAR consisting of", x$meta$N, "observations,",
      x$meta$M, "variables and", x$meta$lags, "lags.")
  cat("\nHyperparameters:",
      paste(x$priors$hyper, collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x$optim$par, 3), collapse = ", "))
  cat("\nIterations (burnt / thinning): ", x$meta$n_draw, " (", x$meta$n_burn,
      "/", x$meta$n_thin, ")", sep = "")
  cat("\nAccepted draws (rate): ", x$accepted, " (",
      round(x$accepted / x$meta$n_draw, 3), ")\n", sep = "")

  if(!is.null(x$irf)) {cat("\n"); print(x$irf)}
  if(!is.null(x$fcast)) {cat("\n"); print(x$fcast)}

  return(invisible(x))
}
