
#' Forecast error variance decompostions draws
#'
#' Computes forecast error variance decompostions (FEVDs) using the impulse
#' response draws obtained from \code{\link{compute_irf}}.
#'
#' @param irf_comp Numeric matrix. Contains a draw of impulse responses
#' obtained from \code{\link{compute_irf}}.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param horizon Integer scalar. Horizon of impulse responses and FEVDs.
#'
#' @return Returns a numeric array of FEVDs.
#'
#' @noRd
compute_fevd <- function(irf_comp, M, horizon) {

  fevd_comp <- apply(irf_comp * irf_comp, c(1, 3), cumsum)
  tmp <- matrix(0, M, M)
  for(i in 1:horizon) {
    tmp <- tmp + tcrossprod(irf_comp[, i, ])
    fevd_comp[i, , ] <- fevd_comp[i, , ] * (1 / diag(tmp))
  }

  return(aperm(fevd_comp, c(2, 1, 3)))
}
