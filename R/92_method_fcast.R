print.bv_fcast <- function(x, ...) {

  if(!inherits(x, "bv_fcast")) {stop()}

  cat("Object with settings for computing forecasts in `bvar()`.\n")

  print_fcast(x, ...)
}


print.bvar_fcast <- function(x, ...) {

  if(!inherits(x, "bvar_fcast")) {stop()}

  cat("Forecast from `bvar()`.\n")

  print_fcast(x, ...)
}

print_fcast <- function(x, ...) {

  cat("\nHorizon: ", x$horizon, "\n", sep = "")

  if(!is.null(x$conditional)) {
    stop("Conditional forecasts not yet implemented")
  }
}
