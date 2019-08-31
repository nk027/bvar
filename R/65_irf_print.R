#' @rdname irf.bvar
#' @export
print.bv_irf <- function(x, ...) {

  if(!inherits(x, "bv_irf")) {stop("Please provide a `bv_irf` object.")}

  cat("Object with settings for computing impulse responses.\n")

  print_irf(x, ...)

  return(invisible(x))
}


#' @rdname irf.bvar
#' @export
print.bvar_irf <- function(x, ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}

  cat("Impulse response object from `bvar()`.\n")

  print_irf(x$setup, ...)

  cat("Variables: ", dim(x[["irf"]])[2], "\n",
      "Iterations: ", dim(x[["irf"]])[1], "\n", sep = "")

  return(invisible(x))
}


#' @rdname irf.bvar
#' @export
print.bvar_fevd <- function(x, digits = 4L, complete = FALSE, ...) {

  if(!inherits(x, "bvar_fevd")) {stop("Please provide a `bvar_fevd` object.")}

  print_coefs(x, digits, type = "FEVD", complete = complete, ...)

  return(invisible(x))
}


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



#' @rdname irf.bvar
#' @export
summary.bvar_irf <- function(
  object,
  vars_impulse = NULL, vars_response = NULL,
  digits = 2L, ...) {

  print.bvar_irf(object)

  quants <- object[["quants"]]
  has_quants <- length(dim(quants)) == 4
  M <- if(has_quants) {dim(quants)[2]} else {M <- dim(quants)[1]}

  variables <- if(is.null(object[["variables"]])) {
    1L:M
  } else {object[["variables"]]}
  pos_imp <- get_var_set(vars_impulse, variables, M)
  pos_res <- get_var_set(vars_response, variables, M)

  cat(if(!has_quants) {"Median impulse responses:\n"} else {"Impulse responses:\n"})
  for(i in pos_res) {
    for(j in pos_imp) {
      cat("    Shock ", variables[j], " on ", variables[i], ":\n", sep = "")
      print(round(if(has_quants) {quants[, i, , j]} else {quants[i, , j]},
                  digits = digits))
    }
  }

  return(invisible(
    if(has_quants) {quants[, pos_res, , pos_imp]} else {quants[pos_res, , pos_imp]}
  ))
}
