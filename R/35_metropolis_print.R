#' @rdname bv_mh
#' @export
print.bv_metropolis <- function(x, ...) {

  if(!inherits(x, "bv_metropolis")) {
    stop("Please provide a `bv_metropolis` object.")
  }

  cat("Object with settings for the Metropolis-Hastings step in `bvar()`.\n",
      "Scaling parameter: ", x[["scale_hess"]], "\n",
      "Automatic acceptance adjustment: ", x[["adjust_acc"]], "\n", sep = "")
  if(x[["adjust_acc"]]) {
    cat("Target acceptance: [", x[["acc_lower"]], ", ", x[["acc_upper"]], "]\n",
        "Change applied: ", x[["acc_loosen"]] - 1, "\n", sep = "")
  }

  return(invisible(x))
}
