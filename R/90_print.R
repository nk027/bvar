#' @export
print.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  y <- x[["meta"]]

  cat("Bayesian VAR consisting of", y[["N"]], "observations,",
      y[["M"]], "variables and", y[["lags"]], "lags.")
  cat("\nTime spent calculating:", format(round(y[["timer"]], 2)))
  cat("\nHyperparameters:",
      paste(x[["priors"]][["hyper"]], collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x[["optim"]][["par"]], 5), collapse = ", "))
  cat("\nIterations (burnt / thinning): ", y[["n_draw"]], " (", y[["n_burn"]],
      " / ", y[["n_thin"]], ")", sep = "")
  cat("\nAccepted draws (rate): ", y[["accepted"]], " (",
      round(y[["accepted"]] / (y[["n_draw"]] - y[["n_burn"]]), 3),
      ")\n", sep = "")

  return(invisible(x))
}
