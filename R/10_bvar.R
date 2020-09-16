
#' Hierarchical Bayesian vector autoregression
#'
#' Used to estimate hierarchical Bayesian Vector Autoregression (VAR) models in
#' the fashion of Giannone, Lenza and Primiceri (2015).
#' Priors are adjusted and added via \code{\link{bv_priors}}.
#' The Metropolis-Hastings step can be modified with \code{\link{bv_mh}}.
#'
#' The model can be expressed as:
#' \deqn{y_t = a_0 + A_1 y_{t-1} + ... + A_p y_{t-p} + \epsilon_t}{y_t = a_0 +
#' A_1 y_{t-1} + ... + A_p y_{t-p} + e_t}
#' See Kuschnig and Vashold (2019) and Giannone, Lenza and Primiceri (2015)
#' for further information.
#' Methods for a \code{bvar} object and its derivatives can be used to:
#' \itemize{
#'   \item predict and analyse scenarios;
#'   \item evaluate shocks and the variance of forecast errors;
#'   \item visualise forecasts and impulse responses, parameters and residuals;
#'   \item retrieve coefficents and the variance-covariance matrix;
#'   \item calculate fitted and residual values;
#' }
#' Note that these methods generally work by calculating quantiles from the
#' posterior draws. The full posterior may be retrieved directly from the
#' objects. The function \code{\link[utils]{str}} can be very helpful for this.
#'
#' @author Nikolas Kuschnig, Lukas Vashold
#'
#' @param data Numeric matrix or dataframe. Note that observations are expected
#' to be ordered from earliest to latest, and variables in the columns.
#' @param lags Integer scalar. Lag order of the model.
#' @param n_draw,n_burn Integer scalar. The number of iterations to (a) cycle
#' through and (b) burn at the start.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th iteration is stored.
#' For a given memory requirement thinning reduces autocorrelation, while
#' increasing effective sample size.
#' @param priors Object from \code{\link{bv_priors}} with prior settings.
#' Used to adjust the Minnesota prior, add custom dummy priors, and choose
#' hyperparameters for hierarchical estimation.
#' @param mh Object from \code{\link{bv_mh}} with settings for the
#' Metropolis-Hastings step. Used to tune automatic adjustment of the
#' acceptance rate within the burn-in period, or manually adjust the proposal
#' variance.
#' @param fcast Object from \code{\link{bv_fcast}} with forecast settings.
#' Options include the horizon and settings for conditional forecasts i.e.
#' scenario analysis.
#' May also be calculated ex-post using \code{\link{predict.bvar}}.
#' @param irf Object from \code{\link{bv_irf}} with settings for the calculation
#' of impulse responses and forecast error variance decompositions. Options
#' include the horizon and different identification schemes.
#' May also be calculated ex-post using \code{\link{irf.bvar}}.
#' @param verbose Logical scalar. Whether to print intermediate results and
#' progress.
#' @param ... Not used.
#'
#' @return Returns a list of class \code{bvar} with the following elements:
#' \itemize{
#'   \item \code{beta} - Numeric array with draws from the posterior of the VAR
#'     coefficients. Also see \code{\link{coef.bvar}}.
#'   \item \code{sigma} - Numeric array with draws from the posterior of the
#'     variance-covariance matrix. Also see \code{\link{vcov.bvar}}.
#'   \item \code{hyper} - Numeric matrix with draws from the posterior of the
#'     hierarchically treated hyperparameters.
#'   \item \code{ml} - Numeric vector with the marginal likelihood (with respect
#'   to the hyperparameters), that determines acceptance probability.
#'   \item \code{optim} - List with outputs of \code{\link[stats]{optim}},
#'     which is used to find starting values for the hyperparameters.
#'   \item \code{prior} - Prior settings from \code{\link{bv_priors}}.
#'   \item \code{call} - Call to the function. See \code{\link{match.call}}.
#'   \item \code{meta} - List with meta information. Includes the number of
#'     variables, accepted draws, number of iterations, and data.
#'   \item \code{variables} - Character vector with the column names of
#'     \emph{data}. If missing, variables are named iteratively.
#'   \item \code{explanatories} - Character vector with names of explanatory
#'     variables. Formatting is akin to: \code{"FEDFUNDS-lag1"}.
#'   \item \code{fcast} - Forecasts from \code{\link{predict.bvar}}.
#'   \item \code{irf} - Impulse responses from \code{\link{irf.bvar}}.
#' }
#'
#' @references
#'   Giannone, D. and Lenza, M. and Primiceri, G. E. (2015) Prior Selection for
#'   Vector Autoregressions. \emph{The Review of Economics and Statistics},
#'   \bold{97:2}, 436-451, \url{https://doi.org/10.1162/REST_a_00483}.
#'
#'   Kuschnig, N. and Vashold, L. (2021) BVAR: Bayesian Vector Autoregressions
#'   with Hierarchical Prior Selection in R.
#'   \emph{Journal of Statistical Software}, \bold{forthcoming}.
#'
#' @seealso \code{\link{bv_priors}}; \code{\link{bv_mh}};
#' \code{\link{bv_fcast}}; \code{\link{bv_irf}};
#' \code{\link{predict.bvar}}; \code{\link{irf.bvar}}; \code{\link{plot.bvar}};
#'
#' @keywords BVAR Metropolis-Hastings MCMC priors hierarchical
#'
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats optim runif quantile
#' @importFrom mvtnorm rmvnorm
#'
#' @examples
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
#'
#' # Calculate and store forecasts and impulse responses
#' predict(x) <- predict(x, horizon = 8)
#' irf(x) <- irf(x, horizon = 8, fevd = FALSE)
#'
#' \dontrun{
#' # Check convergence of the hyperparameters with a trace and density plot
#' plot(x)
#' # Plot forecasts and impulse responses
#' plot(predict(x))
#' plot(irf(x))
#' # Check coefficient values and variance-covariance matrix
#' summary(x)
#' }
bvar <- function(
  data, lags,
  n_draw = 10000L, n_burn = 5000L, n_thin = 1L,
  priors = bv_priors(),
  mh = bv_mh(),
  fcast = NULL,
  irf = NULL,
  verbose = TRUE, ...) {

  cl <- match.call()
  start_time <- Sys.time()


  # Setup and checks -----

  # Data
  if(!all(vapply(data, is.numeric, logical(1L))) ||
     any(is.na(data)) || ncol(data) < 2) {
    stop("Problem with the data. Make sure it is numeric, without any NAs.")
  }

  Y <- as.matrix(data)

  # Integers
  lags <- int_check(lags, min = 1L, max = nrow(Y) - 1, msg = "Issue with lags.")
  n_draw <- int_check(n_draw, min = 10L, msg = "Issue with n_draw.")
  n_burn <- int_check(n_burn, min = 0L, max = n_draw - 1L,
    msg = "Issue with n_burn. Is n_burn < n_draw?")
  n_thin <- int_check(n_thin, min = 1L, max = ((n_draw - n_burn) / 10),
    msg = "Issue with n_thin. Maximum allowed is (n_draw - n_burn) / 10.")
  n_save <- int_check(((n_draw - n_burn) / n_thin), min = 1L)
  verbose <- isTRUE(verbose)

  # Constructors, required
  if(!inherits(priors, "bv_priors")) {
    stop("Please use `bv_priors()` to configure the priors.")
  }
  if(!inherits(mh, "bv_metropolis")) {
    stop("Please use `bv_mh()` to configure the Metropolis-Hastings step.")
  }
  # Not required
  if(!is.null(fcast) && !inherits(fcast, "bv_fcast")) {
    stop("Please use `bv_fcast()` to configure forecasts.")
  }
  if(!is.null(irf) && !inherits(irf, "bv_irf")) {
    stop("Please use `bv_irf()` to configure impulse responses.")
  }

  if(mh[["adjust_acc"]]) {n_adj <- as.integer(n_burn * mh[["adjust_burn"]])}


  # Preparation ---

  X <- lag_var(Y, lags = lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)
  XX <- crossprod(X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)

  variables <- name_deps(variables = colnames(data), M = M)
  explanatories <- name_expl(variables = variables, M = M, lags = lags)


  # Priors -----

  # Minnesota prior ---

  b <- priors[["b"]]
  if(length(b) == 1 || length(b) == M) {
    priors[["b"]] <- matrix(0, nrow = K, ncol = M)
    priors[["b"]][2:(M + 1), ] <- diag(b, M)
  } else if(!is.matrix(b) || !all(dim(b) == c(K, M))) {
    stop("Issue with the prior mean b. Please reconstruct.")
  }

  if(any(priors[["psi"]][["mode"]] == "auto")) {
    psi_temp <- auto_psi(Y, lags)
    priors[["psi"]][["mode"]] <- psi_temp[["mode"]]
    priors[["psi"]][["min"]] <- psi_temp[["min"]]
    priors[["psi"]][["max"]] <- psi_temp[["max"]]
  }
  if(!all(vapply(priors[["psi"]][1:3],
    function(x) length(x) == M, logical(1L)))) {
    stop("Dimensions of psi do not fit the data.")
  }

  # Parameters ---
  pars_names <- names(priors)[ # Exclude reserved names
    !grepl("^hyper$|^var$|^b$|^psi[0-9]+$|^dummy$", names(priors))]
  pars_full <- do.call(c, lapply(pars_names, function(x) priors[[x]][["mode"]]))
  names(pars_full) <- name_pars(pars_names, M)


  # Hierarchical priors ---
  hyper_n <- length(priors[["hyper"]]) +
    sum(priors[["hyper"]] == "psi") * (M - 1)
  if(hyper_n == 0) {stop("Please provide at least one hyperparameter.")}

  get_priors <- function(name, par) {priors[[name]][[par]]}
  hyper <- do.call(c, lapply(priors[["hyper"]], get_priors, par = "mode"))
  hyper_min <- do.call(c, lapply(priors[["hyper"]], get_priors, par = "min"))
  hyper_max <- do.call(c, lapply(priors[["hyper"]], get_priors, par = "max"))
  names(hyper) <- name_pars(priors[["hyper"]], M)

  # Split up psi ---
  for(i in seq_along(priors[["psi"]][["mode"]])) {
    priors[[paste0("psi", i)]] <- vapply(c("mode", "min", "max"), function(x) {
      priors[["psi"]][[x]][i]}, numeric(1L))
  }


  # Optimise and draw -----

  opt <- optim(par = hyper, bv_ml, gr = NULL,
    hyper_min = hyper_min, hyper_max = hyper_max, pars = pars_full,
    priors = priors, Y = Y, X = X, XX = XX, K = K, M = M, N = N, lags = lags,
    opt = TRUE, method = "L-BFGS-B", lower = hyper_min, upper = hyper_max,
    control = list("fnscale" = -1))

  names(opt[["par"]]) <- names(hyper)

  if(verbose) {
    cat("Optimisation concluded.",
      "\nPosterior marginal likelihood: ", round(opt[["value"]], 3),
      "\nHyperparameters: ", paste(names(hyper), round(opt[["par"]], 5),
      sep = " = ", collapse = "; "), "\n", sep = "")
  }


  # Hessian ---

  if(length(mh[["scale_hess"]]) != 1 &&
    length(mh[["scale_hess"]]) != length(hyper)) {
    stop("Length of scale_hess does not match the ", length(hyper),
      " hyperparameters. Please provide a scalar or an element for every ",
      "hyperparameter (see `?bv_mn()`).")
  }

  H <- diag(length(opt[["par"]])) * mh[["scale_hess"]]
  J <- unlist(lapply(names(hyper), function(name) {
    exp(opt[["par"]][[name]]) / (1 + exp(opt[["par"]][[name]])) ^ 2 *
      (priors[[name]][["max"]] - priors[[name]][["min"]])
  }))
  if(any(is.nan(J))) {
    stop("Issue with parameter(s) ",
      paste0(names(hyper)[which(is.nan(J))], collapse = ", "), ". ",
      "Their mode(s) may be too large to exponentiate.")
  }
  if(hyper_n != 1) {J <- diag(J)}
  HH <- J %*% H %*% t(J)

  # Make sure HH is positive definite
  if(hyper_n != 1) {
    HH_eig <- eigen(HH)
    HH_eig[["values"]] <- abs(HH_eig[["values"]])
    HH <- HH_eig
  } else {HH <- list("values" = abs(HH))}


  # Initial draw ---

  while(TRUE) {
    hyper_draw <- rmvn_proposal(n = 1, mean = opt[["par"]], sigma = HH)[1, ]
    ml_draw <- bv_ml(hyper = hyper_draw,
      hyper_min = hyper_min, hyper_max = hyper_max, pars = pars_full,
      priors = priors, Y = Y, X = X, XX = XX, K = K, M = M, N = N, lags = lags)
    if(ml_draw[["log_ml"]] > -1e16) {break}
  }


  # Sampling -----

  # Storage ---
  accepted <- 0 -> accepted_adj # Beauty
  ml_store <- vector("numeric", n_save)
  hyper_store <- matrix(NA, nrow = n_save, ncol = length(hyper_draw),
    dimnames = list(NULL, names(hyper)))
  beta_store <- array(NA, c(n_save, K, M))
  sigma_store <- array(NA, c(n_save, M, M))

  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}

  # Start loop ---
  for(i in seq.int(1 - n_burn, n_draw - n_burn)) {

    # Metropolis-Hastings
    hyper_temp <- rmvn_proposal(n = 1, mean = hyper_draw, sigma = HH)[1, ]
    ml_temp <- bv_ml(hyper = hyper_temp,
      hyper_min = hyper_min, hyper_max = hyper_max, pars = pars_full,
      priors = priors, Y = Y, X = X, XX = XX, K = K, M = M, N = N, lags = lags)

    if(runif(1) < exp(ml_temp[["log_ml"]] - ml_draw[["log_ml"]])) { # Accept
      ml_draw <- ml_temp
      hyper_draw <- hyper_temp
      accepted_adj <- accepted_adj + 1
      if(i > 0) {accepted <- accepted + 1}
    }

    # Tune acceptance during burn-in phase
    if(mh[["adjust_acc"]] && i <= -n_adj && (i + n_burn) %% 10 == 0) {
      acc_rate <- accepted_adj / (i + n_burn)
      if(acc_rate < mh[["acc_lower"]]) {
        HH[["values"]] <- HH[["values"]] * mh[["acc_tighten"]]
      } else if(acc_rate > mh[["acc_upper"]]) {
        HH[["values"]] <- HH[["values"]] * mh[["acc_loosen"]]
      }
    }

    if(i > 0 && i %% n_thin == 0) { # Store draws

      ml_store[(i / n_thin)] <- ml_draw[["log_ml"]]
      hyper_store[(i / n_thin), ] <- hyper_draw

      # Draw parameters, i.e. beta_draw and sigma_draw
      # These need X and N with the dummy observations from `ml_draw`
      draws <- draw_post(XX = ml_draw[["XX"]], N = ml_draw[["N"]],
        M = M, lags = lags, b = priors[["b"]], psi = ml_draw[["psi"]],
        sse = ml_draw[["sse"]], beta_hat = ml_draw[["beta_hat"]],
        omega_inv = ml_draw[["omega_inv"]])

      beta_store[(i / n_thin), , ] <- draws[["beta_draw"]]
      sigma_store[(i / n_thin), , ] <- draws[["sigma_draw"]]

    } # End store

    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}

  } # End loop

  timer <- Sys.time() - start_time

  if(verbose) {
    close(pb)
    cat("Finished MCMC after ", format(round(timer, 2)), ".\n", sep = "")
  }


  # Outputs -----

  out <- structure(list(
    "beta" = beta_store, "sigma" = sigma_store,
    "hyper" = hyper_store, "ml" = ml_store,
    "optim" = opt, "priors" = priors, "call" = cl,
    "variables" = variables, "explanatories" = explanatories,
    "meta" = list("accepted" = accepted, "timer" = timer,
      "Y" = Y, "X" = X, "N" = N, "K" = K, "M" = M, "lags" = lags,
      "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save,
      "n_thin" = n_thin)
    ), class = "bvar")

  if(!is.null(irf)) {
    if(verbose) {cat("Calculating impulse responses.")}
    out[["irf"]] <- tryCatch(irf.bvar(out, irf), error = function(e) {
      warning("\nImpulse response calculation failed with:\n", e)
      return(NULL)})
    if(verbose) {cat("..Done!\n")}
  }
  if(!is.null(fcast)) {
    if(verbose) {cat("Calculating forecasts.")}
    out[["fcast"]] <- tryCatch(predict.bvar(out, fcast), error = function(e) {
      warning("\nForecast calculation failed with:\n", e)
      return(NULL)})
    if(verbose) {cat("..Done!\n")}
  }

  return(out)
}
