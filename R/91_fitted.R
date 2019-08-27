
fitted.bvar <- function(x, ..., conf_bands = 0.16, n_thin = 100L) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  n_pres <- x[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
                      "Problematic value for parameter `n_thin`.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  X <- x[["meta"]][["X"]]
  N <- x[["meta"]][["N"]]
  K <- x[["meta"]][["K"]]
  M <- x[["meta"]][["M"]]
  lags <- x[["meta"]][["lags"]]
  beta <- x[["beta"]]

  Y_fit <- array(NA, c(n_save, N, M))

  j <- 1
  for(i in seq_len(n_save)) {
    beta_comp <- get_beta_comp(beta[j, , ], K, M, lags)
    Y_fit[i, , ] <- (X[, -1] %*% beta_comp +
      c(beta[j, 1, ], rep(0, M * (lags - 1))))[, 1L:M] # Constant
    j <- j + n_thin
  }

  quantiles <- quantile_check(conf_bands)
  Y_fit <- apply(Y_fit, c(2, 3), quantile, quantiles)
  class(Y_fit) <- "bvar_fitted"

  return(Y_fit)
}

print.bvar_fitted <- function(x, ...) {

  cat("Fitted values of a Bayesian VAR with ", dim(x)[2], " observations and ",
      dim(x)[3], " variables.\n", sep = "")
  cat("Computed confidence bands: ",
      paste(dimnames(x)[[1]], collapse = ", "), "\n", sep = "")
  cat("Median fits:\n")
  for(var in seq_len(dim(x)[3])) {
    cat("Variable ", var, ": ",
        paste0(round(x["50%", 1:3, var], 2L), collapse = ", "), ", [...], ",
        paste0(round(x["50%", seq(dim(x)[2] - 2, dim(x)[2]), var], 2L),
               collapse = ", "),
        "\n", sep = "")
  }

  return(invisible(x))
}
