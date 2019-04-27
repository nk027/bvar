#' Impulse response draws
#'
#' Computes impulse responses using the posterior draws of the VAR coefficients
#' and vcov-matrix obtained from \code{\link{draw_post}}.
#'
#' @param beta_comp Numeric matrix. Posterior draw of the VAR coefficients in
#' state space representation.
#' @param sigma Numeric matrix. Posterior draw of the vcov-matrix of the
#' model.
#' @param sigma_chol Numeric matrix. Lower part of the Cholesky decomposition
#' of \emph{sigma}. Calculated as \code{t(chol(sigma))}.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param lags Integer scalar. Number of lags in the model.
#' @param horizon Integer scalar. Horizon for which impulse responses should be
#' computed.
#' @param identification Logical scalar. Whether or not the shocks used for
#' calculating the impulse should be identified. Defaults to \code{TRUE},
#' meaning identification will be performed recursively through a
#' Cholesky decomposition of the vcov-matrix as long as \emph{sign_restr} is
#' \code{NULL}. If set to \code{FALSE}, shocks will be unidentified.
#' @param sign_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1}, \eqn{-1} or \eqn{0}, depending
#' on whether a positive, a negative or no contemporaneous effect of a certain
#' shock is expected.
#' @param fevd Logical scalar. Specifies whether or not forecast error variance
#' decompositions should be calculated.
#'
#' @return Returns a named list containing a numeric matrix of impulse responses
#' and optionally a numeric matrix with the FEVD.
#'
#' @noRd
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
  if(fevd) {out[["fevd"]] <- fevd(irf_comp, M, horizon)}

  return(out)
}
