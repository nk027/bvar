predict.bvar <- function(x, ..., n_draw) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  dots <- list(...)


  # Return existing fcast -------------------------------------------------

  fcast_prev <- x[["fcast"]]

  # If a forecast exists and no settings are provided
  if(!is.null(fcast_prev) && length(dots) == 0L) {return(fcast_prev)}


  # Calculate new fcast ---------------------------------------------------

  fcast <- if(inherits(dots, "bv_fcast")) {dots} else {bv_fcast(...)}

  # If n_draw is provided sample from stored iterations in x
  if(missing(n_draw)) {
    n_draw <- x[["meta"]][["n_save"]]
    iters <- seq(1L, n_draw)
  } else {
    stopifnot(n_draw <= x[["meta"]][["n_save"]])
    iters <- sample(x[["meta"]][["n_save"]], size = n_draw, replace = FALSE)
  }

  Y <- x[["meta"]][["Y"]]
  K <- x[["meta"]][["K"]]
  M <- x[["meta"]][["M"]]
  N <- x[["meta"]][["N"]]
  lags <- x[["meta"]][["lags"]]
  beta <- x[["beta"]]
  sigma <- x[["sigma"]]

  fcast_store <- list(
    "fcast" = array(NA, c(n_draw, fcast[["horizon"]], M)),
    "setup" = fcast
  )
  class(fcast_store) <- "bvar_fcast"

  for(i in seq_along(iters)) {
    beta_comp <- get_beta_comp(beta[iters[i], , ], K, M, lags)
    fcast_store[["fcast"]][i, , ] <- compute_fcast(
      Y = Y, K = K, M = M, N = N, lags = lags,
      horizon = fcast[["horizon"]],
      beta_comp = beta_comp,
      beta_const = beta[iters[i], 1, ], sigma = sigma[iters[i], , ])
  }

  return(fcast_store)
}

irf.bvar <- function(x, settings, thin = 1L) {

  get_beta_comp(beta_draw, K, M, lags)
  get_sigma_chol(sigma_draw)

}

fevd.bvar() <- function(x) {}

summary.bvar() <- function(x) {

  print()
  add_fancy_info()

}

fitted.bvar() <- function(x) {



}

residuals.bvar <- function(x) {

  Y - fitted.bvar(x)

}

coef.bvar <- function(x) {}

vcov.bvar <- function(x) {}

logLik.bvar <- function(x) {}

Phi.bvar <- function(x) {}

Psi.bvar <- function(x) {}

