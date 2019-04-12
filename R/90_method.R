plot.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  hyper_plot(x, ...)

}


print.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  cat("\nBayesian VAR consisting of", x$meta$N, "observations,",
      x$meta$M, "variables and", x$meta$lags, "lags.")
  cat("\nHyperparameters:",
      paste(x$priors$hyper, collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x$optim$par, 3), collapse = ", "))
  cat("\nIterations (burnt / thinning): ", x$meta$n_draw, " (", x$meta$n_burn,
      "/", x$meta$n_thin, ")", sep = "")
  cat("\nAccepted draws (rate): ", x$accepted, " (",
      round(x$accepted / x$meta$n_draw, 3), ")", sep = "")

  if(!is.null(x$irf)) {print(x$irf)}
  if(!is.null(x$fcast)) {print(x$fcast)}

  return(invisible(x))
}


print.bvar_irf <- function(x, ...) {

  if(!inherits(x, "bvar_irf")) {stop()}

  id <- x$setup$identification
  sign_restr <- is.null(x$setup$sign_restr)
  fevd <- is.null(x$fevd)

  cat("\nImpulse responses with a horizon of ", x$setup$horizon, ".", sep = "")
  cat("\nIdentification", if(id) {"via"
      if(sign_restr) {"Choleski decomposition."} else {"sign restrictions."}
    } else {"has been skipped."})
  if(!fevd) {cat("\nForecast error variance decomposition included.")}

  return(invisible(x))
}


print.bvar_fcast <- function(x, ...) {

  if(!inherits(x, "bvar_fcast")) {stop()}

  cat("\nForecast with a horizon of ", x$setup$horizon, ".", sep = "")

  return(invisible(x))
}
