bvar <- function(
  data, lags,
  draws = 10000, burns = 10000, thin = 1,
  mh,
  priors,
  fcast,
  irf,
  sv,
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
  if(!"bvar_prior" %in% class(priors)) stop()
  if(!"bvar_fcast" %in% class(fcast)) stop()
  if(!"bvar_irf" %in% class(irf)) stop()
  if(!"bvar_sv" %in% class(sv)) stop()


  # Preparation -------------------------------------------------------------

  X <- var_lag(Y, lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)


  # Priors ------------------------------------------------------------------

  # Minnesota Prior
  priors$b <- matrix(0, nrow = K, ncol = M)
  priors$b[2:(M + 1), ] <- diag(M)

  if(priors$psi == "auto") priors$psi <- .auto_psi(Y, lags)

  # Check for non-hierarchical estimation
  n_hpriors <- length(priors$hyper)
  if(n_hpriors == 0) .bv_non_hierarchical(...)


  # Optimise ----------------------------------------------------------------

  # Rows: mode, min, max; Cols: hyperparameters
  par_init <- sapply(priors$hyper, function(x) {
    c(priors[[x]]$mode, priors[[x]]$min, priors[[x]]$max)
    })

  opt <- optim(par = par_init[1, ], m_like,
               method = if(n_hpriors == 1) {"Brent"} else {"L-BFGS-B"},
               control = list("fnscale" = -1))


  # Hessian -----------------------------------------------------------------


  # Initial draw ------------------------------------------------------------

  # m_like
  # sv


  # Loop --------------------------------------------------------------------

  # Storage

  # start loop
  if(verbose) pb <- txtProgressBar(min = 0, max = (nburn + nsave), style = 3)
  for(i in (1 - burns):(draws - burns)) {

    # Metropolis-Hastings

    if(runif(1) < 0.5) {
      # accept
    } else {
      # reject
    }

    # tune acceptance

    # saved draws
    if(i > 0 && i %% thin == 0) {
      # store

      # Companion matrix
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
