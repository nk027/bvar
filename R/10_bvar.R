bvar <- function(
  data, lags,
  draws = 10000, burns = 10000, thin = 1,
  priors,
  mh,
  fcast,
  irf,
  verbose = FALSE, ...) {

  # Input Checking ----------------------------------------------------------

  # Data
  if(!is.numeric(data) || any(is.na(data)) || length(data) < 2) {
    stop("Problem with the data. Make sure it's numeric without NAs.")
  } else {
    Y <- as.matrix(data)
  }

  # Integers
  lags <- .int_check(lags, min = 1, max = nrow(Y))
  draws <- .int_check(draws, min = 1)
  burns <- .int_check(burns, min = 0)
  thin <- .int_check(thin, min = 1, max = draws / 10)

  # Constructors
  if(!inherits(priors, "bv_priors")) stop()
  if(!is.null(fcast) && !inherits(fcast, "bv_fcast")) stop()
  if(!is.null(irf) && !inherits(irf, "bv_irf")) stop()


  # Preparation -------------------------------------------------------------

  X <- var_lag(Y, lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)


  # Priors ------------------------------------------------------------------

  # Minnesota prior
  priors$b <- matrix(0, nrow = K, ncol = M)
  priors$b[2:(M + 1), ] <- diag(M)

  if(priors$psi == "auto") priors$psi <- .auto_psi(Y, lags)

  # Parameters
  pars_names <- names(priors)[!names(priors) %in% c("hyper", "var", "b")]
  pars <- do.call(c, lapply(pars_names, function(x) priors[[x]]$mode))
  names(pars) <- .name_pars(pars_names)

  # Dummy priors
  priors$dummy <- pars_names[!pars_names %in% c("lambda", "alpha", "psi")]

  # Hierarchical priors
  hyper_n <- length(priors$hyper) + sum(priors$hyper == "psi") * (M - 1)
  if(hyper_n == 0) bv_non_hierarchical(...)

  hyper <- do.call(c, lapply(priors$hyper, function(x) priors[[x]]$mode))
  hyper_min <- do.call(c, lapply(priors$hyper, function(x) priors[[x]]$min))
  hyper_max <- do.call(c, lapply(priors$hyper, function(x) priors[[x]]$max))
  names(hyper) <- .name_pars(priors$hyper)


  # Optimise ----------------------------------------------------------------

  opt <- optim(par = hyper, bv_ml, pars, priors,
               method = if(hyper_n == 1) {"Brent"} else {"L-BFGS-B"},
               control = list("fnscale" = -1))
  names(opt$par) <- names(hyper_init)


  # Hessian -----------------------------------------------------------------

  H <- diag(length(opt$par)) * mh$scale_hess
  J <- sapply(priors$hyperpars, function(name) {
    exp(opt$par[[name]]) / (1 + exp(opt$par[[name]])) ^ 2 *
      (priors[[name]]$max - priors[[name]]$min)
  })

  if(n_hpriors != 1) {J <- diag(J)}
  HH <- J %*% H %*% t(J)

  # Make sure HH is positive definite
  if(n_hpriors != 1) {
    HH_eig <- eigen(HH)
    HH <- HH_eig$vectors %*% diag(abs(HH_eig$values)) %*% t(HH_eig$vectors)
  } else {HH <- abs(HH)}


  # Initial draw ------------------------------------------------------------

  # m_like


  # Loop --------------------------------------------------------------------

  # Storage

  # start loop
  if(verbose) pb <- txtProgressBar(min = 0, max = (nburn + nsave), style = 3)
  for(i in (1 - burns):(draws - burns)) {

    # Metropolis-Hastings

    if(runif(1) < 0.5) {
      # accept, new draw of estimates with new parameters
      bv_draw(...)
    } else {
      # reject, draw new estimates with old parameters
    }

    # tune acceptance

    # saved draws
    if(i > 0 && i %% thin == 0) {
      # store

      # companion matrix
      # fcast
      # irf
    }
    if(verbose) setTxtProgressBar(pb, (i + nburn))
  }
  # end loop


  # Outputs -----------------------------------------------------------------

  class(out) <- "bvar"
  return(out)
}
