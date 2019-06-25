#' Hierarchical Bayesian Vector Autoregression
#'
#' Hierarchical Bayesian estimation of VAR models in the fashion of Giannone et
#' al. (2015).
#'
#' @param data Numeric matrix or dataframe. Observations need to be ordered
#' from earliest to latest one.
#' @param lags Integer scalar. Number of lags to apply to the data.
#' @param n_draw Integer scalar. Number of total iterations for the model
#' to cycle through.
#' @param n_burn Integer scalar. Number of iterations to discard.
#' @param n_thin Integer scalar. Option to reduce the number of stored
#' iterations to every \emph{n_thin}th one. The number of saved iterations will
#' thus be \code{(n_draw - n_burn) / n_thin}.
#' @param priors \code{bv_priors} object containing priors and their settings.
#' See \code{\link{bv_priors}}.
#' @param mh \code{bv_metropolis} object with settings regarding the acceptance
#' rate of the Metropolis-Hastings step. See \code{\link{bv_mh}}.
#' @param fcast \code{bv_fcast} object. Forecast options set with
#' \code{\link{bv_fcast}}. May be set to \code{NULL} to skip forecasting.
#' @param irf \code{bv_irf} object. Options regarding impulse responses, set via
#' \code{\link{bv_irf}}. May be set to \code{NULL} to skip the calculation of
#' impulse responses.
#' @param verbose Logical scalar. Whether to print intermediate results and
#' progress.
#' @param ... Not used.
#'
#' @return Returns a \code{bvar} object with the following elements:
#' \itemize{
#'   \item \code{beta} - Numeric array with saved draws from the posterior
#'   distribution of the VAR coefficients.
#'   \item \code{sigma} - Numeric array with saved draws from the posterior
#'   distribution of the model's vcov-matrix.
#'   \item \code{hyper} - Numeric matrix with saved draws from the posterior
#'   distributions of hyperparameters of hierarchical priors.
#'   \item \code{ml} - Numeric vector with the value of the posterior marginal
#'   likelihood corresponding to each draw of hyperparameters and associated
#'   VAR coefficients.
#'   \item \code{optim} - List with outputs from \code{\link[stats]{optim}}.
#'   \item \code{prior} - \code{bv_priors} object. See \code{\link{bv_priors}}.
#'   \item \code{call} - Call to the function. See \code{\link{match.call}}.
#'   \item \code{meta} - List with meta information such as number of variables,
#'   accepted draws, number of iterations et cetera.
#'   \item \code{fcast} - \code{bvar_fcast} object with posterior forecast
#'   draws as well as the forecast's setup from \emph{fcast}.
#'   \item \code{irf} - \code{bvar_irf} object with posterior impulse response
#'   draws, as well as the impulse response setup from \emph{irf}.
#' }
#'
#' @references
#'     Giannone, D., Lenza, M., & Primiceri, G. E. (2015). Prior Selection for Vector Autoregressions. Review of Economics and Statistics, 97, 436-451. \url{https://doi.org/10.1162/REST_a_00483}
#'
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats optim runif
#' @importFrom MASS mvrnorm
#'
#' @examples
#' # Access the fred_qd dataset and transform it
#' data("fred_qd")
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' data[5:nrow(data), 1] <- diff(log(data[, 1]), lag = 4) * 100
#' data <- data[5:nrow(data), ]
#'
#' # Compute VAR using 2 lags and a ridiculously low number of draws
#' x <- bvar(
#'   data = data, lags = 1,
#'   n_draw = 500, n_burn = 400, n_thin = 2, verbose = FALSE
#' )
#'
#' # Plot various outputs generated
#' \donttest{
#' plot(x)
#' bv_plot_fcast(x)
#' bv_plot_irf(x)
#' }
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
  if(!inherits(priors, "bv_priors")) {
    stop("Please use `bv_priors()` to configure the priors.")
  }
  if(!inherits(mh, "bv_metropolis")) {
    stop("Please use `bv_mh()` to configure the Metropolis-Hastings step.")
  }
  if(!is.null(fcast) && !inherits(fcast, "bv_fcast")) {
    stop("Please use `bv_fcast()` to configure forecasts.")
  }
  if(!is.null(irf) && !inherits(irf, "bv_irf")) {
    stop("Please use `bv_irf()` to configure impulse responses.")
  }


  # Preparation -------------------------------------------------------------

  X <- lag_var(Y, lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)

  # Check sign restrictions
  if(!is.null(irf[["sign_restr"]]) && length(irf[["sign_restr"]]) != M ^ 2) {
    stop("Length of provided sign restrictions does not fit data.")
  }


  # Priors ------------------------------------------------------------------

  # Minnesota prior
  if(length(priors[["b"]]) == 1 && priors[["b"]] == "auto") {
    priors[["b"]] <- matrix(0, nrow = K, ncol = M)
    priors[["b"]][2:(M + 1), ] <- diag(M)
  } else if(!all(dim(priors[["b"]]) == c(K, M))) {
    stop("Dimensions of prior mean (b) do not match the data.")
  }
  if(priors[["psi"]][["mode"]] == "auto") {
    psi_temp <- auto_psi(Y, lags)
    priors[["psi"]][["mode"]] <- psi_temp[["mode"]]
    priors[["psi"]][["min"]] <- psi_temp[["min"]]
    priors[["psi"]][["max"]] <- psi_temp[["max"]]
  }
  if(!all(vapply(priors[["psi"]][3:length(priors[["psi"]])],
                 function(x) length(x) == M, logical(1)))) {
    stop("Dimensions of psi do not match the data.")
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
    hyper_draw <- mvrnorm(mu = opt[["par"]], Sigma = HH)
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
    hyper_temp <- mvrnorm(mu = hyper_draw, Sigma = HH)
    ml_temp <- bv_ml(hyper = hyper_temp, hyper_min, hyper_max,
                     pars = pars_full, priors, Y, X, K, M, N, lags)

    if(runif(1) < exp(ml_temp[["log_ml"]] - ml_draw[["log_ml"]])) {
      # Accept draw
      ml_draw <- ml_temp
      hyper_draw <- hyper_temp
      accepted_adj <- accepted_adj + 1
      if(i > 0) {accepted <- accepted + 1}

    }

    # Tune acceptance during burn-in phase
    if(mh[["adjust_acc"]] && i <= 0 && (i + n_burn) %% 10 == 0) {
      acc_rate <- accepted_adj / (i + n_burn)
      if(acc_rate < mh[["acc_lower"]]) {
        HH <- HH * mh[["acc_tighten"]]
      } else if(acc_rate > mh[["acc_upper"]]) {
        HH <- HH * mh[["acc_loosen"]]
      }
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
        if(lags > 1) { # Add block-diagonal matrix beneath VAR coefficients
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
    cat("Finished after ", format(round(timer, 2)), ".\n", sep = "")
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
