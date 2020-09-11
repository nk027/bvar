
#' @export
print.bv_priors <- function(x, ...) {

  cat("Object with prior settings for `bvar()`.\n",
    "Hyperparameters: ", paste0(x[["hyper"]], collapse = ", "),
    "\n\n", sep = "")

  if(!is.null(x[["lambda"]])) {print.bv_minnesota(x, indent = TRUE)}

  dummy_pos <- names(x) %in% x[["dummy"]]
  if(any(dummy_pos) && !length(x[["dummy"]]) == 0) {
    cat("\nDummy prior(s):\n")
    dummies <- names(x)[dummy_pos]
    for(dummy in dummies) {
      cat(dummy, ":\n", sep = ""); print.bv_dummy(x[[dummy]], indent = TRUE)
    }
  }

  return(invisible(x))
}


#' @export
print.bv_minnesota <- function(x, indent = FALSE, ...) {

  cat("Minnesota prior:\nlambda:\n"); print(x[["lambda"]], indent = indent)
  cat("alpha:\n"); print(x[["alpha"]], indent = indent)
  cat("psi:\n"); print(x[["psi"]], indent = indent)
  cat("\nVariance of the constant term:", x[["var"]], "\n")

  return(invisible(x))
}


#' @export
print.bv_dummy <- function(x, indent = FALSE, ...) {

  print_priors(x, ...)
  cat(if(indent) {"\t"}, "Mode / Bounds: ",
    x[["mode"]], " / [", x[["min"]], ", ", x[["max"]], "]\n", sep = "")

  return(invisible(x))
}


#' @export
print.bv_psi <- function(x, indent = FALSE, ...) {

  print_priors(x, ...)
  if(any(x[["mode"]] == "auto")) {
    cat(if(indent) {"\t"}, "Mode / Bounds: retrieved automatically\n")
  } else {
    for(i in seq_along(x[["mode"]])) {
      cat(if(indent) {"\t"}, "#", i, " Mode / Bounds: ",
        x[["mode"]][i], " / [", x[["min"]][i], ", ", x[["max"]][i], "]\n",
        sep = "")
    }
  }

  return(invisible(x))
}


#' Priors print method
#'
#' @param x A \code{bv_dummy} or \code{bv_psi} object.
#' @param ... Not used.
#'
#' @noRd
print_priors <- function(x, indent = FALSE, ...) {

  cat(if(indent) {"\t"}, "Shape / Scale: ",
    round(x[["coef"]][["k"]], 3L), " / ",
    round(x[["coef"]][["theta"]], 3L), "\n", sep = "")

  return(invisible(x))
}
