#' @rdname bv_priors
#' @export
print.bv_priors <- function(x, ...) {

  if(!inherits(x, "bv_priors")) {stop("Please provide a `bv_priors` object.")}

  cat("Object with prior settings for `bvar()`.\n",
      "Hyperparameters: ",
      paste0(x[["hyper"]], collapse = ", "),
      "\n\n", sep = "")
  if(!is.null(x[["lambda"]])) {
    cat("Minnesota prior:\nlambda:\n"); print(x[["lambda"]], indent = TRUE)
    cat("alpha:\n"); print(x[["alpha"]], indent = TRUE)
    cat("psi:\n"); print(x[["psi"]], indent = TRUE)
    cat("\nvariance of constant term:", x[["var"]], "\n")
  }
  dummy_pos <- !names(x) %in% c("lambda", "alpha", "psi", "hyper", "var", "b")
  if(any(dummy_pos)) {
    cat("\nDummy prior(s):\n")
    dummies <- names(x)[dummy_pos]
    for(dummy in dummies) {
      cat(dummy, ":\n", sep = ""); print(x[[dummy]], indent = TRUE)
    }
  }

  return(invisible(x))
}


#' @rdname bv_priors
#' @export
print.bv_dummy <- function(x, indent = FALSE, ...) {

  if(!inherits(x, "bv_dummy")) {stop("Please provide a `bv_dummy` object.")}

  print_priors(x, ...)
  cat(if(indent) {"\t"}, "Mode / Bounds: ",
      x[["mode"]], " / [", x[["min"]], ", ", x[["max"]], "]\n", sep = "")

  return(invisible(x))
}


#' @rdname bv_priors
#' @export
print.bv_psi <- function(x, indent = FALSE, ...) {

  if(!inherits(x, "bv_psi")) {stop("Please provide a `bv_psi` object.")}

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
