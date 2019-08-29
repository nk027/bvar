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


#' @rdname predict.bvar
#' @export
summary.bvar_fcast <- function(object, vars = NULL, digits = 2L, ...) {

  print.bvar_fcast(object)

  quants <- object[["quants"]]
  has_quants <- length(dim(quants)) == 3
  M <- if(has_quants) {dim(quants)[3]} else {M <- dim(quants)[2]}

  variables <- if(is.null(object[["variables"]])) {
    1L:M
  } else {object[["variables"]]}
  pos <- get_var_set(vars, variables, M)

  cat(if(!has_quants) {"Median forecast:\n"} else {"Forecast:\n"})
  for(i in pos) {
    cat("\tVariable ", variables[i], ":\n", sep = "")
    print(round(if(has_quants) {quants[, , i]} else {quants[, i]},
                digits = digits))
  }

  return(invisible(if(has_quants) {quants[, , pos]} else {quants[, pos]}))
}
