
#' @export
print.bv_fcast <- function(x, ...) {

  cat("Object with settings for computing forecasts.\n")

  .print_fcast(x, ...)

  return(invisible(x))
}


#' @export
print.bvar_fcast <- function(x, ...) {

  cat("Forecast object from `bvar()`.\n")

  .print_fcast(x[["setup"]], ...)

  cat("Variables: ", dim(x[["fcast"]])[3], "\n",
    "Iterations: ", dim(x[["fcast"]])[1], "\n", sep = "")

  return(invisible(x))
}


#' @noRd
.print_fcast <- function(x, ...) {

  cat("Horizon: ", x[["horizon"]], "\n",
    "Conditional: ", !is.null(x[["cond_path"]]), "\n", sep = "")

  return(invisible(x))
}


#' @rdname predict.bvar
#' @export
summary.bvar_fcast <- function(object, vars = NULL, ...) {

  quants <- object[["quants"]]
  has_quants <- length(dim(quants)) == 3
  M <- if(has_quants) {dim(quants)[3]} else {dim(quants)[2]}

  variables <- name_deps(variables = object[["variables"]], M = M)
  pos <- pos_vars(vars, variables = variables, M = M)

  out <- structure(list(
    "fcast" = object, "quants" = quants,
    "variables" = variables, "pos" = pos, "has_quants" = has_quants),
    class = "bvar_fcast_summary")

  return(out)
}


#' @export
print.bvar_fcast_summary <- function(x, digits = 2L, ...) {

  print.bvar_fcast(x[["fcast"]])

  if(!is.null(x[["fcast"]][["setup"]][["constr_mat"]])) {
    cat("Constraints for conditional forecast:\n")
    print(x[["fcast"]][["setup"]][["constr_mat"]])
  }

  cat(if(!x[["has_quants"]]) {"\nMedian forecast:\n"} else {"\nForecast:\n"})

  for(i in x[["pos"]]) {
    cat("\tVariable ", x[["variables"]][i], ":\n", sep = "")
    print(round(
      if(x[["has_quants"]]) {x[["quants"]][, , i]} else {x[["quants"]][, i]},
      digits = digits))
  }

  return(invisible(x))
}
