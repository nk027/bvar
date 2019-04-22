print.bv_irf <- function(x, ...) {

  if(!inherits(x, "bv_irf")) {stop()}

  cat("Object with settings for computing impulse responses in `bvar()`.\n")

  print_irf(x, ...)
}


print.bvar_irf <- function(x, ...) {

  if(!inherits(x, "bvar_irf")) {stop()}

  cat("Impulse responses from `bvar()`.\n")

  print_irf(x$setup, ...)
}


print_irf <- function(x, ...) {

  cat("Horizon:", x$horizon)

  cat("\nIdentification: ")
  if(x$identification) {
    if(is.null(x$sign_restr)) {
      cat("Cholesky decomposition")
    } else {
      cat("sign restrictions.", "\nChosen restrictions:\n", sep = "")
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
