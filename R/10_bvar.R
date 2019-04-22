bvar <- function(
  data, lags,
  n_draw = 10000, n_burn = 5000, n_thin = 1,
  priors = bv_priors(),
  metropolis = bv_metropolis(),
  fcast = bv_fcast(),
  irf = bv_irf(),
  verbose = FALSE, ...) {

  cl <- match.call()

  # Input Checking ----------------------------------------------------------

  # Data
  if(!all(vapply(data, is.numeric, logical(1))) ||
     any(is.na(data)) || ncol(data) < 2) {
    stop("Problem with the data. Make sure it is numeric without any NAs.")
  }

  Y <- as.matrix(data)
  variables <- colnames(data)

  # Integers
  lags <- int_check(lags, min = 1, max = nrow(Y))
  n_draw <- int_check(n_draw, min = 1)
  n_burn <- int_check(n_burn, min = 0, max = n_draw)
  n_thin <- int_check(n_thin, min = 1, max = ((n_draw - n_burn) / 10))
  n_save <- int_check(((n_draw - n_burn) / n_thin), min = 1)

  # Constructors
  if(!inherits(priors, "bv_priors")) {stop()}
  if(!inherits(metropolis, "bv_metropolis")) {stop()}
  if(!is.null(fcast) && !inherits(fcast, "bv_fcast")) {stop()}
  if(!is.null(irf) && !inherits(irf, "bv_irf")) {stop()}


  # Preparation -------------------------------------------------------------

  X <- lag_var(Y, lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)

  # Check sign restrictions
  if(!is.null(irf[["sign_restr"]]) &&
     length(irf[["sign_restr"]]) != M ^ 2) {stop()}


  # Priors ------------------------------------------------------------------

  # Minnesota prior
  if(length(priors[["b"]]) == 1 && priors[["b"]] == "auto") {
    priors[["b"]] <- matrix(0, nrow = K, ncol = M)
    priors[["b"]][2:(M + 1), ] <- diag(M)
  } else if(!all(dim(priors[["b"]]) == c(K, M))) {
    stop("Dimensions of the prior mean (b) do not match the data.")
  }
  if(length(priors[["psi"]]) == 1 && priors[["psi"]] == "auto") {
    priors[["psi"]] <- auto_psi(Y, lags)
  }
  if(!all(vapply(priors[["psi"]], function(x) length(x) == M, logical(1)))) {
    stop("The dimension of the psi does not match the data.")
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
  if(hyper_n == 0) stop("Non-hierarchical estimation not yet implemented.")

  hyper <- do.call(c, lapply(priors[["hyper"]],
                             function(x) priors[[x]][["mode"]]))
  hyper_min <- do.call(c, lapply(priors[["hyper"]],
                                 function(x) priors[[x]][["min"]]))
  hyper_max <- do.call(c, lapply(priors[["hyper"]],
                                 function(x) priors[[x]][["max"]]))
  names(hyper) <- name_pars(priors[["hyper"]], M)

  # Split up psi
  for(i in 1:length(priors[["psi"]][["mode"]])) {
    priors[[paste0("psi", i)]] <-
      lapply(priors[["psi"]], function(x) x[i])
  }

  # Optimise ----------------------------------------------------------------

  opt <- optim(par = hyper, bv_ml, gr = NULL, hyper_min, hyper_max,
               pars_full, priors, Y, X, K, M, N, lags, opt = TRUE,
               method = if(hyper_n == 1) {"Brent"} else {"L-BFGS-B"},
               lower = hyper_min, upper = hyper_max,
               control = list("fnscale" = -1))
  names(opt[["par"]]) <- names(hyper)


  # Hessian -----------------------------------------------------------------

  H <- diag(length(opt[["par"]])) * metropolis[["scale_hess"]]
  J <- unlist(lapply(names(hyper), function(name) {
    exp(opt[["par"]][[name]]) / (1 + exp(opt[["par"]][[name]])) ^ 2 *
      (priors[[name]][["max"]] - priors[[name]][["min"]])
  }))
  if(hyper_n != 1) {J <- diag(J)}
  HH <- J %*% H %*% t(J)

  # Make sure HH is positive (definite)
  if(hyper_n != 1) {
    HH_eig <- eigen(HH)
    HH <- HH_eig[["vectors"]] %*% diag(abs(HH_eig[["values"]])) %*%
      t(HH_eig[["vectors"]])
  } else {HH <- abs(HH)}


  # Initial draw ------------------------------------------------------------

  while(TRUE) {
    hyper_draw <- MASS::mvrnorm(mu = opt[["par"]], Sigma = HH)
    ml_draw <- bv_ml(hyper = hyper_draw, hyper_min, hyper_max,
                     pars = pars_full, priors, Y, X, K, M, N, lags)
    if(ml_draw[["log_ml"]] > -1e16) break
  }


  # Loop --------------------------------------------------------------------

  # Storage
  accepted <- 0 -> accepted_adj # Beauty
  ml_store <- vector("numeric", n_save)
  hyper_store <- matrix(NA, nrow = n_save, ncol = length(hyper_draw),
                        dimnames = list(NULL, names(hyper)))
  beta_store <- array(NA, c(n_save, K, M))
  sigma_store <- array(NA, c(n_save, M, M))

  if(!is.null(fcast)) {
    fcast_store <- list(
      "fcast" = array(NA, c(n_save, fcast[["horizon"]], M)),
      "setup" = fcast
    )
    class(fcast_store) <- "bvar_fcast"
  }
  if(!is.null(irf)) {
    irf_store <- list(
      "irf" = array(NA, c(n_save, M, irf[["horizon"]], M)),
      "fevd" = if(irf[["fevd"]]) {array(NA, c(n_save, M, M))} else {NULL},
      "setup" = irf
    )
    class(irf_store) <- "bvar_irf"
  }

  # Loop
  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}

  for(i in (1 - n_burn):(n_draw - n_burn)) { # Start loop

    # Metropolis-Hastings
    hyper_temp <- MASS::mvrnorm(mu = hyper_draw, Sigma = HH)
    ml_temp <- bv_ml(hyper = hyper_temp, hyper_min, hyper_max,
                     pars = pars_full, priors, Y, X, K, M, N, lags)

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
    if(metropolis[["adjust_acc"]] && i <= 0 && (i + n_burn) %% 100 == 0) {
      acc_rate <- accepted_adj / 100
      if(acc_rate < metropolis[["lower"]]) {
        HH <- HH * metropolis[["acc_tighten"]]
      } else if(acc_rate > metropolis[["upper"]]) {
        HH <- HH * metropolis[["acc_loosen"]]
      }
      accepted_adj <- 0
    }

    if(i > 0 && i %% n_thin == 0) {

      # Stored iterations
      ml_store[(i / n_thin)] <- ml_draw[["log_ml"]]
      hyper_store[(i / n_thin), ] <- hyper_draw

      # Draw parameters, i.e. beta_draw, sigma_draw & sigma_chol
      # These need X and N including the dummy priors from `ml_draw`
      draws <- draw_post(X = ml_draw[["X"]], N = ml_draw[["N"]],
                         M = M, lags = lags, b = priors[["b"]],
                         psi = ml_draw[["psi"]], sse = ml_draw[["sse"]],
                         beta_hat = ml_draw[["beta_hat"]],
                         omega_inv = ml_draw[["omega_inv"]])

      beta_store[(i / n_thin), , ] <- draws[["beta_draw"]]
      sigma_store[(i / n_thin), , ] <- draws[["sigma_draw"]]

      # Companion matrix is necessary for forecasts and impulse responses
      if(!is.null(fcast) || !is.null(irf)) {
        beta_comp <- matrix(0, K - 1, K - 1)
        beta_comp[1:M, ] <- t(draws[["beta_draw"]][2:K, ])
        if(lags > 1) { # Add diagonal matrix
          beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
        }
      }

      # Forecast
      if(!is.null(fcast)) {
        beta_const <- draws[["beta_draw"]][1, ]
        fcast_store[["fcast"]][(i / n_thin), , ] <- compute_fcast(
          Y = Y, K = K, M = M, N = N, lags = lags,
          horizon = fcast[["horizon"]],
          beta_comp = beta_comp, beta_const = beta_const,
          sigma = draws[["sigma_draw"]])
      } # End forecast

      # Impulse responses
      if(!is.null(irf)) {
        irf_comp  <- compute_irf(
          beta_comp = beta_comp,
          sigma = draws[["sigma_draw"]], sigma_chol = draws[["sigma_chol"]],
          M = M, lags = lags,
          horizon = irf[["horizon"]], identification = irf[["identification"]],
          sign_restr = irf[["sign_restr"]], fevd = irf[["fevd"]])
        irf_store[["irf"]][(i / n_thin), , , ] <- irf_comp[["irf"]]
        if(irf[["fevd"]]) {
          irf_store[["fevd"]][(i / n_thin), , ] <- irf_comp[["fevd"]]
        }
      } # End impulse responses

    }

    if(verbose) setTxtProgressBar(pb, (i + n_burn))

  } # End loop

  close(pb)


  # Outputs -----------------------------------------------------------------

  out <- list("beta" = beta_store, "sigma" = sigma_store,
              "hyper" = hyper_store, "ml" = ml_store,
              "accepted" = accepted, "optim" = opt, "priors" = priors,
              "variables" = variables, "call" = cl)

  out[["meta"]] <- list("N" = N, "M" = M, "lags" = lags, "n_draw" = n_draw,
                        "n_burn" = n_burn, "n_save" = n_save, "n_thin" = n_thin)

  if(!is.null(fcast)) out[["fcast"]] <- fcast_store
  if(!is.null(irf)) out[["irf"]] <- irf_store

  class(out) <- "bvar"

  return(out)
}
