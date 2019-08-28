#' @rdname bv_priors
#' @export
print.bv_priors <- function(x, ...) {

  if(!inherits(x, "bv_priors")) {stop("Please provide a `bv_priors` object.")}

  cat("Object with prior settings for `bvar()`.\n",
      "Hyperparameters: ",
      paste0(x[["hyper"]], collapse = ", "),
      "\n", sep = "")
  if(!is.null(x[["lambda"]])) {
    cat("\tlambda:\n"); print(x[["lambda"]])
    cat("\talpha:\n"); print(x[["alpha"]])
    cat("\tpsi:\n"); print(x[["psi"]])
    cat("\tvariance:", x[["var"]], "\n")
  }
  dummy_pos <- !names(x) %in% c("lambda", "alpha", "psi", "hyper", "var", "b")
  if(any(dummy_pos)) {
    dummies <- names(x)[dummy_pos]
    for(dummy in dummies) {cat("\t", dummy, ":\n", sep = ""); print(x[[dummy]])}
  }

  return(invisible(x))
}


#' @rdname bv_dummy
#' @export
print.bv_dummy <- function(x, ...) {

  if(!inherits(x, "bv_dummy")) {stop("Please provide a `bv_dummy` object.")}

  print_priors(x, ...)
  cat("Mode / Bounds = ",
      x[["mode"]], " / [", x[["min"]], ", ", x[["max"]], "]\n", sep = "")

  return(invisible(x))
}


#' @rdname bv_mn
#' @export
print.bv_psi <- function(x, ...) {

  if(!inherits(x, "bv_psi")) {stop("Please provide a `bv_psi` object.")}

  print_priors(x, ...)
  for(i in seq_along(x[["mode"]])) {
    cat("#", i, " Mode / Bounds = ",
        x[["mode"]][i], " / [", x[["min"]][i], ", ", x[["max"]][i], "]\n",
        sep = "")
  }

  return(invisible(x))
}


#' Priors print method
#'
#' @param x A \code{bv_dummy} or \code{bv_psi} object.
#' @param ... Not used.
#'
#' @noRd
print_priors <- function(x, ...) {

  cat("Shape / Scale = ",
      round(x[["coef"]][["k"]], 3L), " / ",
      round(x[["coef"]][["theta"]], 3L), "\n", sep = "")

  return(invisible(x))
}
