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
#' in \emph{hyper} are overwritten with their hierarchical counterpart.
#' @param priors List created via \code{\link{bv_priors}}. Contains information
#' on the Minnesota prior and optional dummy priors (that are named in
#' \code{priors$dummy}).
#' @param Y Numeric \eqn{N * M} matrix.
#' @param X Numeric \eqn{N * K} matrix.
#' @param K Integer scalar. Columns of \emph{X}, i.e. \eqn{M * lags + 1}.
#' @param M Integer scalar. Columns of \emph{Y}, i.e. number of variables.
#' @param N Integer scalar. Rows of \emph{Y}, alternatively \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param opt Optional logical scalar. Determines whether the return value is
#' a numeric scalar or a list. Used to call \code{\link{bv_ml}} in
#' \code{\link[stats]{optim}}.
#'
#' @return Returns a list with the following objects by default:
#' \itemize{
#'   \item \code{log_ml} - A numeric scalar with the log-posterior.
#'   \item \code{X}, \code{N} - The lagged data matrix with possible dummy
#'   priors appended and its number of rows. Necessary for drawing from
#'   posterior distributions with \code{\link{draw_post}}.
#'   \item \code{psi}, \code{sse}, \code{beta_hat}, \code{omega_inv} - Further
#'   values necessary for drawing from posterior distributions.
#' }
#' If opt is \code{TRUE} only the value of \code{log_ml} is returned.
#'
#' @importFrom stats dgamma
#'
#' @noRd
bv_ml <- function(
  hyper, hyper_min = -Inf, hyper_max = Inf,
  pars, priors, Y, X, K, M, N, lags,
  opt = FALSE) {

  # Bounds ------------------------------------------------------------------

  if(any(hyper_min > hyper | hyper > hyper_max)) {
      if(opt) {return(-1e18)} else {return(list("log_ml" = -1e18))}
  }


  # Priors ------------------------------------------------------------------

  # Overwrite passed parameters with hyperparameters
  for(name in unique(names(hyper))) {
    pars[names(pars) == name] <- hyper[names(hyper) == name]
  }

  psi <- diag(pars[grep("^psi[0-9]*", names(pars))])
  omega <- vector("numeric", 1 + M * lags)
  omega[1] <- priors[["var"]]
  for(i in 1:lags) {
    omega[seq(2 + M * (i - 1), 1 + i * M)] <-
      pars[["lambda"]] ^ 2 / i ^ pars[["alpha"]] /
      pars[grep("^psi[0-9]*", names(pars))]
  }

  # Dummy priors
  if(length(priors[["dummy"]]) > 0) {
    dmy <- lapply(priors[["dummy"]], function(x) {
      tryCatch(priors[[x]][["fun"]](Y = Y, lags = lags, par = pars[[x]]),
               error = function(e) {
                 message("Issue with generating dummy observations for ",
                         x, ". Make sure the function works properly.")
                 stop(e)})
    })
    Y <- rbind(do.call(rbind,
                       lapply(dmy, function(x) matrix(x[["Y"]], ncol = M))), Y)
    X <- rbind(do.call(rbind,
                       lapply(dmy, function(x) matrix(x[["X"]], ncol = K))), X)
    N <- nrow(Y)
  }


  # Calc --------------------------------------------------------------------

  omega_inv <- diag(1 / omega)
  XX <- crossprod(X)
  beta_hat <- solve(XX + omega_inv) %*% (crossprod(X, Y) +
                                           omega_inv %*% priors[["b"]])
  sse <- crossprod(Y - X %*% beta_hat)
  psi_inv <- solve(sqrt(psi))
  omega_ml <- diag(sqrt(omega)) %*% XX %*% diag(sqrt(omega))
  psi_ml <- psi_inv %*%
    (sse + t(beta_hat - priors[["b"]]) %*% omega_inv %*%
       (beta_hat - priors[["b"]])) %*% psi_inv

  # Eigenvalues + 1 as another way of computing the determinants
  omega_ml_ev <- Re(eigen(omega_ml, only.values = TRUE)[["values"]])
  omega_ml_ev[omega_ml_ev < 1e-12] <- 0
  omega_ml_ev <- omega_ml_ev + 1
  psi_ml_ev <- Re(eigen(psi_ml, only.values = TRUE)[["values"]])
  psi_ml_ev[psi_ml_ev < 1e-12] <- 0
  psi_ml_ev <- psi_ml_ev + 1

  # Likelihood
  log_ml <- (-M * N * log(pi) / 2) +
    sum(lgamma(((N + M + 2) - 0:(M - 1)) / 2) -
        lgamma(((M + 2) - 0:(M -1)) / 2)) -
    (N * sum(log(diag(psi))) / 2) - (M * sum(log(omega_ml_ev)) / 2) -
    ((N + M + 2) * sum(log(psi_ml_ev)) / 2)

  # Add prior-pdfs
  if(length(priors[["dummy"]]) > 0)
    log_ml <- log_ml + sum(sapply(priors[["dummy"]], function(x) {
      log(dgamma(pars[[x]],
                 shape = priors[[x]][["coef"]][["k"]],
                 scale = priors[[x]][["coef"]][["theta"]]))
    }))


  # Output ------------------------------------------------------------------

  if(opt) {return(log_ml)}

  # Return log_ml and objects necessary for drawing
  return(list("log_ml" = log_ml, "X" = X, "N" = N, "psi" = psi,
              "sse" = sse, "beta_hat" = beta_hat, "omega_inv" = omega_inv))
}
