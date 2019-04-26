#' Hierarchical Bayesian Vector Autoregression
#'
#' @param data Numeric matrix or dataframe. Each column contains a variable,
#' observations have to be ordered from earliest to latest one.
#' @param lags Integer scalar. Number of lags to be used when estimating the
#' model.
#' @param n_draw Integer scalar. Number of total iterations that the model
#' cycles through including the iterations to be bured (see \code{n_burn}).
#' @param n_burn Integer scalar. Number of iterations that are discarded and
#' whose associated draws from the posterior distributions are thus not saved.
#' @param n_thin Integer scalar. Option to reduce the number of stored
#' iterations. The number of saved iterations will be calculated as
#' \code{(n_draw - n_burn) / n_thin}.
#' @param priors \code{bv_priors} object. See \code{\link{bv_priors}}.
#' @param mh \code{bv_metropolis} object. See \code{\link{bv_mh}}.
#' @param fcast \code{bv_fcast} object. See \code{\link{bv_fcast}}.
#' @param irf \code{bv_irf} object. See \code{\link{bv_irf}}.
#' @param verbose Logical scalar. Whether to print intermediate results and
#' progress.
#' @param ... Not used.
#'
#' @return Returns a \code{bvar} object, that may be displayed using
#' \code{\link{print.bvar}}. The object contains the following elements:
#' \itemize{
#'   \item beta - Saved draws from the posterior distribution of the VAR
#'   coefficients.
#'   \item sigma - Saved draws from the posterior distribution of the
#'   vcov-matrix of the model.
#'   \item hyper - Saved draws from the posterior distributions of the
#'   hyperparameters of the priors that were included.
#'   \item ml - The value of the marginal likelihood corresponding to each draw
#'   of the hyperparameters of the priors and the associated VAR coefficients.
#'   \item optim - Object coming from \code{\link[stats]{optim}} containing the
#'   best set of starting values for the hyperparameters of the priors used as
#'   well as associated components. See \code{\link[stats]{optim}} for more
#'   information.
#'   \item prior - \code{bv_priors} object. See \code{\link{bv_priors}}.
#'   \item call - The original call to the \code{bvar} function.
#'   \item meta - Meta information regarding the model such as number of
#'   variables, accepted draws, number of iterations etc.
#'   \item fcast - Stores the posterior draws of forecasts if they are computed
#'   as well as information regarding them like the forecasting horizon.
#'   \item irf - Stores the posterior draws of impulse responses if the are
#'   computed as well as information regarding them like the reponse horizon or
#'   the identification method.
#' }
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats optim runif
#'
#' @examples
#' # Access the fred_qd dataset and transform it
#' data("fred_qd")
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' data[5:nrow(data), 1] <- diff(log(data[, 1]), lag = 4) * 100
#' data <- data[5:nrow(data), ]
#'
#' # Compute VAR using 5 lags and standard settings
#' x <- bvar(data = data, lags = 5)
#'
#' # Plot various outputs generated
#' plot(x)
#' bv_plot_fcast(x)
#' bv_plot_irf(x)
bvar <- function(
  data, lags,
  n_draw = 10000, n_burn = 5000, n_thin = 1,
  priors = bv_priors(),
  mh = bv_mh(),
  fcast = bv_fcast(),
  irf = bv_irf(),
  verbose = TRUE, ...) {

  cl <- match.call()
  start_time <- Sys.time()

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
  if(!inherits(mh, "bv_metropolis")) {stop()}
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
  if(hyper_n == 0) {stop("Non-hierarchical estimation not yet implemented.")}

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

  opt <- optim(
    par = hyper, bv_ml, gr = NULL,
    hyper_min = hyper_min, hyper_max = hyper_max, pars = pars_full,
    priors = priors, Y = Y, X = X, K = K, M = M, N = N, lags = lags, opt = TRUE,
    method = "L-BFGS-B", lower = hyper_min, upper = hyper_max,
    control = list("fnscale" = -1)
  )
  names(opt[["par"]]) <- names(hyper)
  if(verbose) {
    cat("Optimisation concluded.",
        "\nPosterior marginal likelihood: ", round(opt[["value"]], 3),
        "\nParameters: ",
        paste(names(hyper), round(opt[["par"]], 2),
              sep = " = ", collapse = "; "), "\n", sep = "")
  }


  # Hessian -----------------------------------------------------------------

  H <- diag(length(opt[["par"]])) * mh[["scale_hess"]]
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
    if(ml_draw[["log_ml"]] > -1e16) {break}
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
    }

    # Tune acceptance during burn-in phase
    if(mh[["adjust_acc"]] && i <= 0 && (i + n_burn) %% 100 == 0) {
      acc_rate <- accepted_adj / 100
      if(acc_rate < mh[["lower"]]) {
        HH <- HH * mh[["acc_tighten"]]
      } else if(acc_rate > mh[["upper"]]) {
        HH <- HH * mh[["acc_loosen"]]
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

    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}

  } # End loop

  timer <- Sys.time() - start_time

  if(verbose) {
    close(pb)
    cat("Finished after ", format(timer), ".\n", sep = "")
  }


  # Outputs -----------------------------------------------------------------

  out <- list("beta" = beta_store, "sigma" = sigma_store,
              "hyper" = hyper_store, "ml" = ml_store,
              "optim" = opt, "priors" = priors,
              "variables" = variables, "call" = cl)

  out[["meta"]] <- list("accepted" = accepted, "N" = N, "M" = M, "lags" = lags,
                        "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save,
                        "n_thin" = n_thin,
                        "timer" = timer)

  if(!is.null(fcast)) {out[["fcast"]] <- fcast_store}
  if(!is.null(irf)) {out[["irf"]] <- irf_store}

  class(out) <- "bvar"

  return(out)
}
