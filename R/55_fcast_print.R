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

  if(!inherits(object, "bvar_fcast")) {
    stop("Please provide a `bvar_fcast` object.")
  }

  quants <- object[["quants"]]
  has_quants <- length(dim(quants)) == 3
  M <- if(has_quants) {dim(quants)[3]} else {M <- dim(quants)[2]}

  variables <- if(is.null(object[["variables"]])) {
    1L:M
  } else {object[["variables"]]}
  pos <- get_var_set(vars, variables, M)

  out <- list(
    "fcast" = object,
    "quants" = quants,
    "variables" = variables,
    "pos" = pos,
    "has_quants" = has_quants
  )
  class(out) <- "bvar_fcast_summary"

  return(out)

  return(invisible(if(has_quants) {quants[, , pos]} else {quants[, pos]}))
}


#' @rdname predict.bvar
#' @export
print.bvar_fcast_summary <- function(x, digits = 2L, ...) {

  if(!inherits(x, "bvar_fcast_summary")) {
    stop("Please provide a `bvar_fcast_summary` object.")
  }

  print.bvar_fcast(x$fcast)
    
  cat(if(!x$has_quants) {"Median forecast:\n"} else {"Forecast:\n"})

  for(i in x$pos) {
    cat("\tVariable ", x$variables[i], ":\n", sep = "")
    print(round(if(x$has_quants) {x$quants[, , i]} else {x$quants[, i]},
                digits = digits))
  }

  return(invisible(x))
}
