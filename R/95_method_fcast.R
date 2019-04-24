#' @rdname print.bvar
#' @export
print.bv_fcast <- function(x, ...) {

  if(!inherits(x, "bv_fcast")) {stop()}

  cat("Object with settings for computing forecasts in `bvar()`.\n")

  print_fcast(x, ...)
}


#' @rdname print.bvar
#' @export
print.bvar_fcast <- function(x, ...) {

  if(!inherits(x, "bvar_fcast")) {stop()}

  cat("Forecast from `bvar()`.\n")

  print_fcast(x$setup, ...)
}


#' Forecast print method
#'
#' @param x A \code{bv_fcast} object.
#' @param ... Not used.
print_fcast <- function(x, ...) {

  cat("Horizon: ", x$horizon, "\n", sep = "")

  if(x$conditional) {
    stop("Conditional forecasts not yet implemented")
  }
}
