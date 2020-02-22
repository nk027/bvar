#' Forecast computation
#'
#' Compute unconditional forecasts from the VAR's posterior draws obtained via
#' \code{\link{draw_post}}.
#'
#' @param Y Numeric matrix (\eqn{N * M}).
#' @param K Integer scalar. Columns of \emph{X}, i.e. \eqn{M * lags + 1}.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param N Integer scalar. Rows of \emph{Y}, alternatively \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients of
#' the model in state space representation.
#' @param beta_const Numeric vector. Posterior draw of the VAR coefficients
#' corresponding to the constant of the model.
#' @param sigma Numeric matrix. Posterior draw of the vcov-matrix of the
#' model.
#' @param conditional Logical scalar. Whether or not the produced forecasts
#' will be used for conditional forecasts (i.e. forecasts without shocks).
#'
#' @return Returns a matrix containing forecasts for all variables in the model.
#'
#' @importFrom stats rnorm
#'
#' @noRd
compute_fcast <- function(
  Y, K, M, N, lags,
  horizon,
  beta_comp, beta_const,
  sigma,
  conditional = FALSE) {

  Y_f <- matrix(NA, horizon + 1, K - 1)
  Y_f[1, ] <- vapply(t(Y[N:(N - lags + 1), ]), c, numeric(1L))

  for(i in seq.int(2, 1 + horizon)) {
    Y_f[i, ] <- Y_f[i - 1, ] %*% t(beta_comp) +
      c(beta_const, rep(0, M * (lags - 1)))
  }

  if(!conditional) { # Add noise
    Y_f[-1, 1:M] <- Y_f[-1, 1:M] +
      t(sigma %*% matrix(rnorm(M * horizon), nrow = M))
  }

  # Remove Y_t and lagged variables
  return(Y_f[2:(1 + horizon), 1:M])
}


#' Conditional forecast computation
#'
#' Compute conditional forecasts using algorithm by Waggoner and Zha (1999).
#'
#' @param cond_mat Matrix containing constrained paths of variables and
#' \code{NAs} for unrestricted values.
#' @param noshock_fcast Matrix containing unconditional forecasts without
#' shocks computed by \code{\link{compute_fcast}}.
#' @param ortho_irf Matrix containing orthogonal impulse responses for all
#' variables computed by \code{\link{compute_irf}} or \code{\link{irf.bvar}}.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param M Integer scalar. Columns of \emph{Y}.
#'
#' @return Returns a matrix containing conditional forecasts for all variables
#' in the model.
#'
#' @references
#'     Waggoner, D. F., & Zha, T. (1999). Conditional forecasts in dynamic multivariate models. Review of Economics and Statistics, 81(4), 639-651. \url{https://doi.org/10.1162/003465399558508}
#'
#' @seealso \code{\link{compute_fcast}}; \code{\link{compute_irf}};
#' \code{\link{irf.bvar}}
#'
#' @importFrom stats rnorm
#'
#' @noRd
get_cond_fcast <- function(cond_mat,
  noshock_fcast, ortho_irf, horizon, M) {

  cond_fcast <- matrix(NA, horizon, M)
  # First get constrained shocks
  v <- sum(!is.na(cond_mat))
  s <- M * horizon
  r <- c()
  R <- matrix(0, 0, s)

  for(i in seq_len(horizon)) {
    for(j in seq_len(M)) {
      if(is.na(cond_mat[i, j])) {next}
      r <- c(r, (cond_mat[i, j] - noshock_fcast[i, j]))
      R <- rbind(R, c(rep(0, s)))
      for(k in 1:i) {
        R[nrow(R), ((k - 1) * M + 1):(k * M)] <- ortho_irf[j, (i - k + 1) , ]
      }
    }
  }

  R_svd <- svd(R, nu = nrow(R), nv = ncol(R))
  U <- R_svd[["u"]]
  P_inv <- diag(1/R_svd[["d"]])
  V1 <- R_svd[["v"]][, 1:v]
  V2 <- R_svd[["v"]][, (v + 1):s]
  eta <- V1 %*% P_inv %*% t(U) %*% r + V2 %*% rnorm(s - v)
  eta <- matrix(eta, M, horizon)

  # Use constrained shocks and unconditional forecasts (without shocks) to
  # create conditional forecasts
  for(h in seq_len(horizon)) {
    temp <- matrix(0, M, 1)
    for(k in seq_len(h)) {
      temp <- temp + ortho_irf[, (h - k + 1), ] %*% eta[ , k]
    }
    cond_fcast[h, ] <- noshock_fcast[h, ] + t(temp)
  }

  return(cond_fcast)
}
