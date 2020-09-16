
#' Log-posterior of a BVAR
#'
#' Compute the log-posterior (or log-marginal-likelihood) of a Bayesian VAR
#' with a Minnesota prior and optional dummy priors. Prior parameters may be
#' treated hierarchically. Create objects necessary for drawing from the
#' posterior distributions of coefficients and covariance matrix of the
#' residuals.
#'
#' @param hyper Named numeric vector. Hyperparameters for hierarchical
#' estimation.
#' @param hyper_min,hyper_max Optional numeric vector. Minimum / maximum values
#' allowed for hyperparameters. If these are breached a value of -1e18 is
#' returned.
#' @param pars Named numeric vector with prior parameters. Values also found
#' in \emph{hyper} are overwritten with their hierarchical counterparts.
#' @param Y Numeric \eqn{N * M} matrix.
#' @param X Numeric \eqn{N * K} matrix.
#' @param XX Numeric \eqn{K * K} matrix. Crossproduct of \emph{X}, used to save
#' matrix calculations when no dummy priors are included.
#' @param K Integer scalar. Columns of \emph{X}, i.e. \eqn{M * lags + 1}.
#' @param M Integer scalar. Columns of \emph{Y}, i.e. number of variables.
#' @param N Integer scalar. Rows of \emph{Y}, alternatively \emph{X}.
#' @param opt Optional logical scalar. Determines whether the return value is
#' a numeric scalar or a list. Used to call \code{\link{bv_ml}} in
#' \code{\link[stats]{optim}}.
#' @inheritParams bvar
#'
#' @return Returns a list by default, containing the following objects:
#' \itemize{
#'   \item \code{log_ml} - A numeric scalar with the log-posterior.
#'   \item \code{XX}, \code{N} - The crossproduct of the lagged data matrix,
#'   potentially with dummy priors and the number of rows including them.
#'   Necessary for drawing from posterior distributions with
#'   \code{\link{draw_post}}.
#'   \item \code{psi}, \code{sse}, \code{beta_hat}, \code{omega_inv} - Further
#'   values necessary for drawing from posterior distributions.
#' }
#' If opt is \code{TRUE} only a numeric scalar with \code{log_ml} is returned.
#'
#' @importFrom stats dgamma
#'
#' @noRd
bv_ml <- function(
  hyper, hyper_min = -Inf, hyper_max = Inf,
  pars, priors, Y, X, XX, K, M, N, lags,
  opt = FALSE) {

  # Check bounds ---

  if(any(hyper_min > hyper | hyper > hyper_max)) {
    if(opt) {return(-1e18)} else {return(list("log_ml" = -1e18))}
  }


  # Priors -----

  # Overwrite passed parameters with hyperparameters if provided
  for(name in unique(names(hyper))) {
    pars[names(pars) == name] <- hyper[names(hyper) == name]
  }

  psi_vec <- pars[grep("^psi[0-9]*", names(pars))]
  psi <- diag(psi_vec)
  omega <- vector("numeric", 1 + M * lags)
  omega[1] <- priors[["var"]]
  for(i in seq.int(1, lags)) {
    omega[seq.int(2 + M * (i - 1), 1 + i * M)] <- pars[["lambda"]] ^ 2 /
      i ^ pars[["alpha"]] / psi_vec
  }

  # Dummy priors
  if(length(priors[["dummy"]]) > 0) {
    dmy <- lapply(priors[["dummy"]], function(x) {
      tryCatch(priors[[x]][["fun"]](Y = Y, lags = lags, par = pars[[x]]),
        error = function(e) {
          message("Issue generating dummy observations for ",
            x, ". Make sure the provided function works properly.")
          stop(e)})
    })
    Y_dmy <- do.call(rbind, lapply(dmy, function(x) matrix(x[["Y"]], ncol = M)))
    X_dmy <- do.call(rbind, lapply(dmy, function(x) matrix(x[["X"]], ncol = K)))
    N_dummy <- nrow(Y_dmy)
    Y <- rbind(Y_dmy, Y)
    X <- rbind(X_dmy, X)
    XX <- crossprod(X)
    N <- nrow(Y)
  }


  # Calc -----

  omega_inv <- diag(1 / omega)
  psi_inv <- diag(1 / sqrt(psi_vec))
  omega_sqrt <- diag(sqrt(omega))
  b <- priors[["b"]]

  # Likelihood ---
  ev_full <- calc_ev(omega_inv = omega_inv, omega_sqrt = omega_sqrt,
    psi_inv = psi_inv, X = X, XX = XX, Y = Y, b = b, beta_hat = TRUE)
  log_ml <- calc_logml(M = M, N = N, psi = psi,
    omega_ml_ev = ev_full[["omega"]], psi_ml_ev = ev_full[["psi"]])

  # Add priors
  log_ml <- log_ml + sum(vapply(
    priors[["hyper"]][which(!priors$hyper == "psi")], function(x) {
      dgamma(pars[[x]],
        shape = priors[[x]][["coef"]][["k"]],
        scale = priors[[x]][["coef"]][["theta"]], log = TRUE)
  }, numeric(1L)))

  if(any(priors[["hyper"]] == "psi")) {
    psi_coef <- priors[["psi"]][["coef"]]
    log_ml <- log_ml + sum(vapply(
      names(pars)[grep("^psi[0-9]*", names(pars))], function(x) {
        p_log_ig(pars[[x]],
          shape = psi_coef[["k"]], scale = psi_coef[["theta"]])
    }, numeric(1L)))
  }

  if(length(priors[["dummy"]]) > 0) {
    ev_dummy <- calc_ev(omega_inv = omega_inv, omega_sqrt = omega_sqrt,
      psi_inv = psi_inv, X = X_dmy, XX = NULL, Y = Y_dmy, b = b,
      beta_hat = FALSE)
    log_ml <- log_ml - calc_logml(M = M, N = N_dummy, psi = psi,
      omega_ml_ev = ev_dummy[["omega"]], psi_ml_ev = ev_dummy[["psi"]])
  }


  # Output -----

  if(opt) {return(log_ml)} # For optim

  # Return log_ml and objects necessary for drawing
  return(
    list("log_ml" = log_ml, "XX" = XX, "N" = N, "psi" = psi,
      "sse" = ev_full[["sse"]], "beta_hat" = ev_full[["beta_hat"]],
      "omega_inv" = omega_inv)
  )
}
