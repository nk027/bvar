#' Impulse response settings
#'
#' Function to provide settings for the computation of impulse repsonses to
#' \code{\link{bvar}}. Provides options for setting the horizon for which
#' impulse responses should be computed, whether or not forecast error variance
#' decompositions (FEVDs) should be computed and, if and what kind of
#' identification should be used (Cholesky decomposition and sign restrictions
#' are supported).
#'
#' @param horizon Integer scalar. Specifies the horizon for which impulse
#' responses (and FEVDs) should be computed. Set to 12 by default.
#' @param fevd Logical scalar. Whether or not forecast error variance
#' decompositions should be calculated. Defaults to FALSE.
#' @param identification Logical scalar. Whether or not the shocks used for
#' calculating impulses should be identified. Defaults to TRUE, i.e.
#' identification via Cholesky decomposition unless sign_restr are provided.
#' @param sign_restr Numeric matrix. Sign restrictions for identification.
#' Elements should be set to \eqn{1} (\eqn{-1}) to restrict for positive
#' (negative) impacts. If no presumption about the impact can be made the
#' corresponding elements can be set to \eqn{0}. The default value is NULL,
#' meaning identification would be done through Cholesky decomposition.
#'
#' @return Returns a named list of class bv_irf with options for
#' \code{\link{bvar}}.
#' @export
#'
#' @examples
#' # Set impulse responses to a horizon of 20 time periods and enable FEVD
#' # (Identification by means of a Cholesky decomposition)
#' bv_irf(horizon = 20, fevd = TRUE)
#'
#' # Identify impulse responses using sign restrictions
#' data("fred_qd")
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' data[5:nrow(data), 1] <- diff(log(data[, 1]), lag = 4) * 100
#' data <- data[5:nrow(data), ]
#'
#' # Signs should be based on economic theory
#' signs <- matrix(c(1, 1, -1, -1, 1, -1, -1, 1, 1), nrow = 3)
#'
#' irf_signs <- bv_irf(sign_restr = signs)
#'
#' bvar(data, lags = 5, irf = irf_signs)
bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon.")

  if(!is.logical(c(identification, fevd))){
    stop("Please provide fevd and identification as logical scalars.")
  }

  if(!is.null(sign_restr) && !all(sign_restr %in% c(-1, 0, 1)) &&
     sqrt(length(sign_restr)) %% 1 != 0) {
    stop("Please provide sign_restr as a numeric square matrix containing ",
         "0s, 1s and -1s.")
  }
  if(is.vector(sign_restr)) {
    sign_restr <- matrix(sign_restr, nrow = sqrt(length(sign_restr)))
  }

  out <- list("horizon" = horizon, "fevd" = fevd,
              "identification" = identification, "sign_restr" = sign_restr)
  class(out) <- "bv_irf"

  return(out)
}
