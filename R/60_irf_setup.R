#' Impulse response settings
#'
#' Provide settings for the computation of impulse responses to
#' \code{\link{bvar}}, \code{\link{irf.bvar}} or \code{\link{fevd.bvar}}. Allows
#' setting the horizon for which impulse responses should be computed, whether
#' or not forecast error variance decompositions (FEVDs) should be included
#' and if and what kind of identification should be used.
#'
#' Identification can be performed via Cholesky decomposition and sign
#' restrictions. The algorithm for generating suitable sign restrictions
#' follows Rubio-Ramirez et al. (2010).
#' Note the possiblity of finding no suitable sign restrictions.
#'
#' @param horizon Integer scalar. The horizon for which impulse responses
#' (and FEVDs) should be computed.
#' @param fevd Logical scalar. Whether or not forecast error variance
#' decompositions should be calculated.
#' @param identification Logical scalar. Whether or not the shocks used for
#' calculating impulses should be identified. Defaults to \code{TRUE}, i.e.
#' identification via Cholesky decomposition unless \emph{sign_restr} is
#' provided.
#' @param sign_restr Numeric matrix. Sign restrictions for identification.
#' Elements should be set to \eqn{1} (\eqn{-1}) to restrict for positive
#' (negative) impacts. If no presumption about the impact can be made the
#' corresponding elements can be set to \eqn{0}. The default value is
#' \code{NULL}, meaning identification would be performed via Cholesky
#' decomposition.
#' @param sign_lim Integer scalar. Maximum number of rotational matrices to
#' draw and check for fitting sign restrictions.
#'
#' @return Returns a named list of class \code{bv_irf} with options for
#' \code{\link{bvar}}, \code{\link{irf.bvar}} or \code{\link{fevd.bvar}}.
#'
#' @references
#'     Rubio-Ramirez, J. F., Waggoner, D. F., & Zha, T. (2010). Structural Vector Autoregressions: Theory of Identification and Algorithms for Inference. The Review of Economic Studies, 77, 665-696. \url{https://doi.org/10.1111/j.1467-937X.2009.00578.x}
#'
#' @seealso \code{\link{bvar}}; \code{\link{irf.bvar}};
#' \code{\link{plot.bvar_irf}}
#'
#' @keywords VAR BVAR irf impulse responses fevd settings
#'
#' @export
#'
#' @examples
#' # Set impulse responses to a horizon of 20 time periods and enable FEVD
#' # (Identification is performed via Cholesky decomposition)
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
#' \donttest{
#' bvar(data, lags = 5, irf = irf_signs)
#' }
bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL,
  sign_lim = 1000) {

  # Input checks
  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")
  sign_lim <- int_check(sign_lim, min = 1, max = 1e6,
                        msg = "Invalid value for sign_lim (outside of [1, 1e6]).")

  if(!is.logical(c(identification, fevd))){
    stop("Please provide fevd and identification as logical scalars.")
  }

  if(fevd) {
    message("Note that forecast error variance decompositions are being ",
      "extended for the next release. Return values may differ.")
  }

  if(!is.null(sign_restr) && !is.numeric(sign_restr) &&
    !all(sign_restr %in% c(-1, 0, NA, 1)) &&
    sqrt(length(sign_restr)) %% 1 != 0) {
    stop("Please provide sign_restr as a numeric square matrix containing ",
      "NAs, 1s and -1s.")
  }
  if(0 %in% sign_restr) {
    warning("Please set unrestricted elements to NA instead of 0. ",
      "This functionality is being deprecated for zero-sign restrictions.")
    sign_restr[sign_restr == 0] <- NA_integer_
  }

  # Outputs
  out <- list("horizon" = horizon, "fevd" = fevd,
              "identification" = identification,
              "sign_restr" = sign_restr, "sign_lim" = sign_lim)
  class(out) <- "bv_irf"

  return(out)
}
