#' @rdname bv_fcast
#' @export
print.bv_fcast <- function(x, ...) {

  if(!inherits(x, "bv_fcast")) {stop("Please provide a `bv_fcast` object.")}

  cat("Object with settings for computing forecasts.\n")

  print_fcast(x, ...)

  return(invisible(x))
}


#' @rdname predict.bvar
#' @export
print.bvar_fcast <- function(x, vars = NULL, ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}

  cat("Forecast object from `bvar()`.\n")

  print_fcast(x$setup, ...)

  cat("Variables: ", dim(x[["fcast"]])[3], "\n",
      "Iterations: ", dim(x[["fcast"]])[1], "\n", sep = "")

  return(invisible(x))
}


#' @noRd
print_fcast <- function(x, ...) {

  cat("Horizon: ", x[["horizon"]], "\n", sep = "")

  return(invisible(x))
}


#' Forecast summary method
#'
#' Print and return quantiles of forecasts from \code{\link{bvar}}.
#'
#' @return Returns an array with the desired forecast quantiles invisibly.
#'
#' @export
#'
#' @rdname predict.bvar
summary.bvar_fcast <- function(object, vars = NULL, digits = 2L, ...) {

  print.bvar_fcast(object)

  quants <- object[["quants"]]
  variables <- if(is.null(object[["variables"]])) {
    1L:dim(quants)[3]
  } else {object[["variables"]]}
  pos <- get_var_set(vars, variables, dim(quants)[3])

  cat("Forecast:\n")
  for(i in pos) {
    cat("\tVariable ", variables[i], ":\n", sep = "")
    print(round(quants[, , i], digits = digits))
  }

  return(invisible(quants[, , pos]))
}
