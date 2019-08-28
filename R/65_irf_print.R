#' @rdname print.bvar
#' @export
print.bv_irf <- function(x, ...) {

  if(!inherits(x, "bv_irf")) {stop("Please provide a `bv_irf` object.")}

  cat("Object with settings for computing impulse responses in `bvar()`.\n")

  print_irf(x, ...)
}


#' @rdname print.bvar
#' @export
print.bvar_irf <- function(x, ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}

  cat("Impulse response object from `bvar()`.\n")

  print_irf(x$setup, ...)

  cat("Variables: ", dim(x[["irf"]])[2], "\n",
      "Iterations: ", dim(x[["irf"]])[1], "\n", sep = "")
}

#' @rdname print.bvar
#' @export
print.bv_fevd <- function() {

  if(!inherits(x, "bv_fevd")) {stop("Please provide a `bv_fevd` object.")}

}

print.bvar_fevd <- function() {

  if(!inherits(x, "bvar_fevd")) {stop("Please provide a `bvar_fevd` object.")}


}


#' Impulse response print method
#'
#' @param x A \code{bv_irf} object.
#' @param ... Not used.
#'
#' @noRd
print_irf <- function(x, ...) {

  cat("Horizon:", x$horizon)

  cat("\nIdentification: ")
  if(x$identification) {
    if(is.null(x$sign_restr)) {
      cat("Cholesky decomposition")
    } else {
      cat("Sign restrictions", "\nChosen restrictions:\n", sep = "")
      sign_restr <- apply(x$sign_restr, 2, factor,
                          levels = c(-1, 0, 1), labels = c("-", "0", "+"))
      if(length(sign_restr) < 10 ^ 2) {
        cat("\t\t\tShock to\n\t\t\t", # Use cat
            paste0("Var", 1:nrow(sign_restr), sep = "\t"),
            paste0(c("\nResponse of\t", rep("\n\t\t", nrow(sign_restr) - 1)),
                   "Var", 1:nrow(sign_restr), "\t",
                   apply(sign_restr, 1, function(x)
                     paste0(" ", x, sep = "\t", collapse = "")),
                   collapse = "\n"))
      } else if(length(sign_restr) < 18 ^ 2) {
        print(sign_restr) # Print the matrix
      } else {
        cat("Too large to print.") # Skip
      }
    }
  } else {
    cat(FALSE)
  }

  cat("\nFEVD: ", x$fevd, "\n", sep = "")

  return(invisible(x))
}


#' Impulse response functions summary method
#'
#' Print and return quantiles of impulse response functions from
#' \code{\link{bvar}}.
#'
#' @param x A \code{bvar_fcast} object.
#' @param vars_impulse Optional numeric or character vector. Used to subset the
#' plot's impulses to certain variables by position or name (must be available).
#' Defaults to \code{NULL}, i.e. all variables.
#' @param vars_response Optional numeric or character vector. Used to subset the
#' plot's responses to certain variables by position or name (must be
#' available). Defaults to \code{NULL}, i.e. all variables.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param ... Not used.
#'
#' @return Returns an array with the desired impulse response quantiles
#' invisibly.
#'
#' @noRd
summary.bvar_irf <- function(x,
                             vars_impulse = NULL, vars_response = NULL,
                             digits = 2L, ...) {

  print.bvar_irf(x)

  quants <- x[["quants"]]
  variables <- if(is.null(x[["variables"]])) {
    1L:dim(quants)[2]
  } else {x[["variables"]]}
  pos_imp <- get_var_set(vars_impulse, variables, dim(quants)[2])
  pos_res <- get_var_set(vars_response, variables, dim(quants)[2])


  cat("Impulse Responses:\n")
  for(i in pos_res) {
    for(j in pos_imp) {
      cat("\tShock ", variables[j], " on ", variables[i], ":\n", sep = "")
      print(round(quants[, i, , j], digits = digits))
    }
  }

  return(invisible(quants[, pos_res, , pos_imp]))
}
