#' Impulse response draws
#'
#' Computes impulse responses using the posterior draws of the VAR coefficients
#' and vcov-matrix obtained from `bv_draw()`.
#'
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients of
#' the model in state space representation.
#' @param sigma_draw Numeric matrix. Posterior draw of the vcoc-matrix of the
#' model.
#' @param sigma_chol Numeric matrix. Lower part of the Choleski decomposition
#' of sigma_draw. Calculated as \code{t(chol(sigma_draw))}.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Specifies the horizon for which impulse
#' responses should be computed.
#' @param identification Logical scalar. Specifies whether or not the shocks
#' used for calculating the impulse should be identified. Default is TRUE,
#' identification will then be achieved recursively i.e. through a Cholesky
#' decomposition of the vcov-matrix if \emph{sign_restr} is NULL. If set to
#' FALSE, shocks will be unidentified.
#' @param sign_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1}, \eqn{-1} or \eqn{0}, depending
#' on whether a positive, a negative or no contemporaneous effect of a certain
#' shock is expected.
#' @param fevd Logical scalar. Specifies whether or not forecast error variance
#' decompositions should be calculated.
#'
#' @return Returns a matrix used as shock for computations of impulse responses
#' identified via sign restrictions.

compute_irf <- function(
  beta_comp,
  sigma, sigma_chol,
  M, lags,
  horizon,
  identification,
  sign_restr,
  fevd) {

  # Identification
  shock <- if(identification) {
    if(is.null(sign_restr)) {
      sigma_chol
    } else {sign_restr(sigma_chol, sign_restr, M)}
  } else {sigma}

  # IRF
  irf_comp <- array(0, c(M * lags, horizon, M * lags))
  irf_comp[1:M, 1, 1:M] <- shock
  for(i in 2:horizon) {
    irf_comp[, i, ] <- irf_comp[, i - 1, ] %*% t(beta_comp)
  }
  irf_comp <- irf_comp[1:M, , 1:M]
  out <- list("irf" = irf_comp)

  # FEVD
  if(fevd) out[["fevd"]] <- fevd(irf_comp, M, horizon)

  return(out)
}
