#' @rdname print.bvar
#' @export
print.bv_fcast <- function(x, ...) {

  if(!inherits(x, "bv_fcast")) {stop("Please provide a `bv_fcast` object.")}

  cat("Object with settings for computing forecasts in `bvar()`.\n")

  print_fcast(x, ...)

  return(invisible(x))
}


#' @rdname print.bvar
#' @export
print.bvar_fcast <- function(x, vars = NULL, ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}

  cat("Forecast object from `bvar()`.\n")

  print_fcast(x$setup, ...)

  cat("Variables: ", dim(x[["fcast"]])[3], "\n",
      "Iterations: ", dim(x[["fcast"]])[1], "\n", sep = "")

  return(invisible(x))
}


#' Forecast print method
#'
#' @param x A \code{bv_fcast} or \code{bvar_fcast} object.
#' @param ... Not used.
#'
#' @noRd
print_fcast <- function(x, ...) {

  cat("Horizon: ", x[["horizon"]], "\n", sep = "")

  return(invisible(x))
}


#' Forecast summary method
#'
#' Print and return quantiles of forecasts from \code{\link{bvar}}.
#'
#' @param x A \code{bvar_fcast} object.
#' @param vars Optional numeric or character vector. Used to subset the output
#' to certain variables by position or name (must be available). Defaults to
#' \code{NULL}, i.e. all variables.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param ... Not used.
#'
#' @return Returns an array with the desired forecast quantiles invisibly.
#'
#' @noRd
summary.bvar_fcast <- function(x, vars = NULL, digits = 2L, ...) {

  print.bvar_fcast(x)

  quants <- x[["quants"]]
  variables <- if(is.null(x[["variables"]])) {
    1L:dim(quants)[3]
  } else {x[["variables"]]}
  pos <- get_var_set(vars, variables, dim(quants)[3])

  cat("Forecast:\n")
  for(i in pos) {
    cat("\tVariable ", variables[i], ":\n", sep = "")
    print(round(quants[, , i], digits = digits))
  }

  return(invisible(quants[, , pos]))
}
