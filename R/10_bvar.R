bvar <- function(
  data, lags,
  n_draw = 10000, n_burn = 5000, thin = 1,
  priors = bv_priors(),
  metropolis = bv_metropolis(),
  fcast = NULL,
  irf = NULL,
  verbose = FALSE, ...) {

  # Input Checking ----------------------------------------------------------

  # Data
  if(!is.numeric(data) || any(is.na(data)) || length(data) < 2) {
    stop("Problem with the data. Make sure it's numeric without NAs.")
  } else {Y <- as.matrix(data)}

  # Integers
  lags <- int_check(lags, min = 1, max = nrow(Y))
  n_draw <- int_check(n_draw, min = 1)
  n_burn <- int_check(n_burn, min = 0, max = n_draw)
  thin <- int_check(thin, min = 1, max = n_draw / 10)

  # Constructors
  if(!inherits(priors, "bv_priors")) stop()
  if(!inherits(metropolis, "bv_metropolis")) stop()
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
  priors[["b"]] <- matrix(0, nrow = K, ncol = M)
  priors[["b"]][2:(M + 1), ] <- diag(M)
  if(length(priors[["psi"]]) == 1 && priors[["psi"]] == "auto") {
    priors[["psi"]] <- auto_psi(Y, lags)
  }

  # Parameters
  pars_names <- names(priors)[!names(priors) %in% c("hyper", "var", "b")]
  pars_full <- do.call(c, lapply(pars_names, function(x) priors[[x]][["mode"]]))
  names(pars_full) <- name_pars(pars_names, M)

  # Dummy priors
  priors[["dummy"]] <- pars_names[!pars_names %in% c("lambda", "alpha", "psi")]

  # Hierarchical priors
  hyper_n <- length(priors[["hyper"]]) +
    sum(priors[["hyper"]] == "psi") * (M - 1)
  if(hyper_n == 0) bv_non_hierarchical(...)

  hyper <- do.call(c, lapply(priors[["hyper"]],
                             function(x) priors[[x]][["mode"]]))
  hyper_min <- do.call(c, lapply(priors[["hyper"]],
                                 function(x) priors[[x]][["min"]]))
  hyper_max <- do.call(c, lapply(priors[["hyper"]],
                                 function(x) priors[[x]][["max"]]))
  names(hyper) <- name_pars(priors[["hyper"]], M)


  # Optimise ----------------------------------------------------------------

  opt <- optim(par = hyper, bv_ml, gr = NULL, hyper_min, hyper_max,
               pars_full, priors, Y, X, K, M, N, lags, opt = TRUE,
               method = if(hyper_n == 1) {"Brent"} else {"L-BFGS-B"},
               lower = hyper_min, upper = hyper_max,
               control = list("fnscale" = -1))
  names(opt[["par"]]) <- names(hyper)


  # Hessian -----------------------------------------------------------------

  H <- diag(length(opt[["par"]])) * metropolis[["scale_hess"]]
  J <- sapply(priors[["hyper"]], function(name) {
    exp(opt[["par"]][[name]]) / (1 + exp(opt[["par"]][[name]])) ^ 2 *
      (priors[[name]][["max"]] - priors[[name]][["min"]])
  })
  if(hyper_n != 1) J <- diag(J)
  HH <- J %*% H %*% t(J)

  # Make sure HH is positive definite
  if(hyper_n != 1) {
    HH_eig <- eigen(HH)
    HH <- HH_eig[["vectors"]] %*% diag(abs(HH_eig[["values"]])) %*%
      t(HH_eig[["vectors"]])
  } else {HH <- abs(HH)}


  # Initial draw ------------------------------------------------------------

  draw_necessary <- TRUE
  while(draw_necessary) {
    hyper_draw <- MASS::mvrnorm(mu = opt[["par"]], Sigma = HH)
    ml_draw <- bv_ml(hyper = hyper_draw, hyper_min, hyper_max, pars = pars_full,
                     priors, Y, X, K, M, N, lags)
    if(ml_draw[["log_ml"]] > -1e16) draw_necessary <- FALSE
  }


  # Loop --------------------------------------------------------------------

  # Storage
  accepted <- accepted_adj <- 0
  ml_store <- vector("numeric", (n_draw / thin) - n_burn)
  hyper_store <- matrix(NA,
                        nrow = (n_draw / thin) - n_burb,
                        ncol = length(hyper_draw),
                        dimnames = list(NULL, names(hyper)))
  beta_store <- vector("list", (n_draw / thin) - n_burn)
  sigma_store <- vector("list", (n_draw / thin) - n_burn)

  # Loop
  if(verbose) pb <- txtProgressBar(min = 0, max = n_draw, style = 3)

  for(i in (1 - n_burn):(n_draw - n_burn)) { # Start loop

    # Metropolis-Hastings
    hyper_temp <- MASS::mvrnorm(mu = hyper_draw, Sigma = HH)
    ml_temp <- bv_ml(hyper = hyper_temp, hyper_min, hyper_max, pars = pars_full,
                     priors, Y, X, K, M, N, lags)

    if(runif(1) < exp(ml_temp[["log_ml"]] - ml_draw[["log_ml"]])) {
      # Accept draw
      ml_draw <- ml_temp
      hyper_draw <- hyper_temp
      accepted <- accepted + 1
      accepted_adj <- accepted_adj + 1
    } else {
      # Reject draw
    }

    # Tune acceptance during burn-in phase
    if(metropolis[["adjust_acc"]] && i < 0 && (i + n_burn) %% 100 == 0) {
      acc_rate <- accepted_adj / 100
      if(acc_rate < metropolis[["lower"]]) {
        HH <- HH * metropolis[["acc_tighten"]]
      } else if(acc_rate > metropolis[["upper"]]) {
        HH <- HH * metropolis[["acc_loosen"]]
      }
      accepted_adj <- 0
    }

    if(i > 0 && i %% thin == 0) {
      # Stored iterations

      ml_store[(i / thin)] <- ml_draw[["log_ml"]]
      hyper_store[(i / thin), ] <- hyper_draw
      # Draw parameters, i.e. beta_draw, sigma_draw & sigma_chol
      draws <- bv_draw(Y = ml_draw[["Y"]], X = ml_draw[["X"]],
                       N = ml_draw[["N"]], lags = lags, M = M, priors[["b"]],
                       psi = ml_draw[["psi"]], sse = ml_draw[["sse"]],
                       beta_hat = ml_draw[["beta_hat"]],
                       omega_inv = ml_draw[["omega_inv"]])

      beta_store[[(i / thin)]] <- draws[["beta_draw"]]
      sigma_store[[(i / thin)]] <- draws[["sigma_draw"]]

      # Companion matrix is necessary for forecasts and impulse responses
      if(!is.null(fcast) || !is.null(irf)) {
        beta_comp <- matrix(0, K - 1, K - 1)
        beta_comp[1:M, ] <- t(draws[["beta_draw"]][2:K, ])
        if(lags > 1) {
          beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
        }
      }

      # Forecast
      if(!is.null(irf)) {

      }

      # IRF
      if(!is.null(irf)) {
        irf_draw  <- bv_irf(beta_comp = beta_comp,
                            sigma_draw = draws[["sigma_draw"]],
                            M = M, lags = lags,
                            irf_hor = irf[["irf_hor"]],
                            irf_id = irf[["irf_id"]],
                            irf_signs = irf[["irf_signs"]],
                            fevd = irf[["fevd"]])
      }
    }

    if(verbose) setTxtProgressBar(pb, (i + n_burn))

  } # End loop

  close(pb)

  # Outputs -----------------------------------------------------------------

  out <- list("beta" = beta_store, "sigma" = sigma_store,
              "hyper" = hyper_store, "ml" = ml_store,
              "accepted" = accepted, "optim" = opt)
  class(out) <- "bvar"

  return(out)
}
