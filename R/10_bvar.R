bvar <- function(
  data, lags,
  draws = 10000, burns = 10000, thin = 1,
  priors = bv_priors(),
  metropolis = bv_metropolis(),
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
  pars_full <- do.call(c, lapply(pars_names, function(x) priors[[x]]$mode))
  names(pars_full) <- .name_pars(pars_names)

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

  opt <- optim(par = hyper, bv_ml, gr = NULL, hyper_min, hyper_max,
               pars_full, priors, Y, X, K, M, N, lags, opt = TRUE,
               method = if(hyper_n == 1) {"Brent"} else {"L-BFGS-B"},
               lower = hyper_min, upper = hyper_max,
               control = list("fnscale" = -1))
  names(opt$par) <- names(hyper)


  # Hessian -----------------------------------------------------------------

  H <- diag(length(opt$par)) * metropolis$scale_hess
  J <- sapply(priors$hyper, function(name) {
    exp(opt$par[[name]]) / (1 + exp(opt$par[[name]])) ^ 2 *
      (priors[[name]]$max - priors[[name]]$min)
  })

  if(hyper_n != 1) J <- diag(J)
  HH <- J %*% H %*% t(J)

  # Make sure HH is positive definite
  if(hyper_n != 1) {
    HH_eig <- eigen(HH)
    HH <- HH_eig$vectors %*% diag(abs(HH_eig$values)) %*% t(HH_eig$vectors)
  } else {HH <- abs(HH)}


  # Initial draw ------------------------------------------------------------

  draw_necessary <- TRUE
  while(draw_necessary) {
    hyper_draw <- MASS::mvrnorm(mu = opt$par, Sigma = HH)
    ml_draw <- bv_ml(hyper = hyper_draw, hyper_min, hyper_max, pars = pars_full,
                     priors, Y, X, K, M, N, lags)
    if(ml_draw$log_ml > -1e16) draw_necessary <- FALSE
  }


  # Loop --------------------------------------------------------------------

  # Storage
  ml_store <- vector("numeric", (draws / thin))
  hyper_store <- matrix(NA, nrow = (draws / thin), ncol = length(hyper_draw),
                        dimnames = list(NULL, names(hyper)))
  beta_store <- vector("list", (draws / thin))
  sigma_store <- vector("list", (draws / thin))



  if(verbose) pb <- txtProgressBar(min = 0, max = (nburn + nsave), style = 3)

  for(i in (1 - burns):(draws - burns)) { # Start loop

    # Metropolis-Hastings
    hyper_temp <- MASS::mvrnorm(mu = hyper_draw, Sigma = HH)
    ml_temp <- bv_ml(hyper = hyper_temp, hyper_min, hyper_max, pars = par_full,
                     priors, Y, X, K, M, N, lags)

    if(runif(1) < exp(ml_temp$log_ml - ml_temp$log_ml)) {
      # Accept draw
      ml_draw <- ml_temp
      hyper_draw <- hyper_temp
      accepted <- accepted + 1
      accepted_adj <- accepted_adj + 1
    } else {
      # Reject draw
    }

    # Tune acceptance during burn-in phase
    if(metropolis[["adjust_acc"]] && i < 0 && (i + nburn) %% 100 == 0) {
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

      beta_store[[(i / thin)]] <- draws$beta_draw
      sigma_store[[(i / thin)]] <- draws$sigma_draw

      # Companion matrix is necessary for forecasts and impulse responses
      if(!is.null(fcast) || !is.null(irf)){
        beta_comp <- matrix(0, K - 1, K - 1)
        beta_comp[1:M, ] <- t(draws[["beta_draw"]][2:K, ])
        if(lags > 1) {
          beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
        }
      }

      # fcast

      # irf
      if(!is.null(irf)){
        irf_draw  <- bv_irf(beta_comp = beta_comp,
                            sigma_draw = draws[["sigma_draw"]],
                            M = M, lags = lags,
                            irf_hor = irf[["irf_hor"]],
                            irf_id = irf[["irf_id"]],
                            irf_signs = irf[["irf_signs"]],
                            fevd = irf[["fevd"]])

      }



      # fcast
    }

    if(verbose) setTxtProgressBar(pb, (i + nburn))

  } # End loop

  close(pb)

  # Outputs -----------------------------------------------------------------

  class(out) <- "bvar"
  return(out)
}
