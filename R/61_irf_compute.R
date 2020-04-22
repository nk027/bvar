#' Impulse response draws
#'
#' Computes impulse responses using the posterior draws of the VAR coefficients
#' and VCOV-matrix obtained from \code{\link{draw_post}}.
#'
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients in
#' state space representation.
#' @param sigma Numeric matrix. Posterior draw of the VCOV-matrix of the
#' model.
#' @param sigma_chol Numeric matrix. Lower part of the Cholesky decomposition
#' of \emph{sigma}. Calculated as \code{t(chol(sigma))}.
#' @param M Integer scalar. Number of columns in \emph{Y}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Horizon for which impulse responses should be
#' computed. Note that the first period corresponds to impacts i.e.
#' contemporaneous effects.
#' @param identification Logical scalar. Whether or not the shocks used for
#' calculating the impulse should be identified. Defaults to \code{TRUE},
#' meaning identification will be performed recursively through a
#' Cholesky decomposition of the VCOV-matrix as long as \emph{sign_restr} is
#' \code{NULL}. If set to \code{FALSE}, shocks will be unidentified.
#' @param sign_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either 1, -1 or \code{NA} depending on
#' whether a positive, negative, or no contemporaneous effect of a certain shock
#' is expected.
#' @param sign_lim Integer scalar. Maximum number of rotational matrices to
#' draw and check for fitting sign restrictions.
#'
#' @return Returns a numeric array of impulse responses.
#'
#' @noRd
compute_irf <- function(
  beta_comp,
  sigma, sigma_chol,
  M, lags,
  horizon,
  identification,
  sign_restr, sign_lim) {

  # Identification
  shock <- if(identification) {
    if(is.null(sign_restr)) {
      sigma_chol <- t(chol(sigma))
    } else {
      sign_restr(sigma_chol = t(chol(sigma)), sign_restr = sign_restr,
        M = M, sign_lim = sign_lim)
    }
  } else {sigma}

  # Impulse responses
  irf_comp <- array(0, c(M * lags, horizon, M * lags))
  irf_comp[1:M, 1, 1:M] <- shock
  for(i in 2:horizon) {
    irf_comp[, i, ] <- beta_comp %*% irf_comp[, i - 1, ] # Could vectorise
  }
  irf_comp <- irf_comp[1:M, , 1:M]

  return(irf_comp)
}
