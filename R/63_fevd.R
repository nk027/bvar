#' Forecast error variance decompostions draws
#'
#' Computes Forecast error variance decompostions (FEVDs) using the draw of
#' impulse responses obtained by \code{\link{compute_irf}}.
#'
#' @param irf_comp Numeric matrix. Contains a draw of impulse responses
#' obtained by \code{\link{compute_irf}}.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param horizon Integer scalar. Specifies the horizon for which impulse
#' responses and corresponsing FEVDs should be computed.
#'
#' @return Returns impulse reponses using the posterior draws of the VAR
#' coefficients and the vcov-matrix of the model either to identified shocks,
#' using a Cholesky decomposition or sign restrictions, or unidentified ones.
fevd <- function(irf_comp, M, horizon) {

  fevd_comp <- aperm(apply(irf_comp * irf_comp, c(1, 3), cumsum), c(2, 3, 1))
  accm <- matrix(0, M, M)
  for(i in 1:horizon) {
    accm <- accm + irf_comp[, i, ] %*% t(irf_comp[, i, ])
    denm <- matrix((diag(accm)), M, M)
    fevd_comp[, , i] <- fevd_comp[, , i] / denm
  }

  return(apply(fevd_comp, c(1, 2), mean, na.rm = TRUE))
}
