#' Impulse response settings
#'
#' Function to provide settings for the computation of impulse repsonses to
#' \code{\link{bvar}}. Provides options for setting the horizon for which
#' impulse responses should be computed, whether or not forecast error variance
#' decompositions should be computed and, if and what identification is to be
#' used (either Cholesky decomposition or sign restrictions). If sign
#' restrictions are chosen as means of identification, a matrix containing the
#' expected impacts of the variables under investigation must be provided.
#'
#' @param horizon Integer scalar. Specifies the horizon for which impulse
#' responses should be computed.
#' @param fevd Logical scalar. Specifies whether or not forecast error variance
#' decompositions (FEVDs) should be calculated.
#' @param identification Logical scalar. Specifies whether or not the shocks
#' used for calculating the impulse should be identified. Default is
#' \code{TRUE}, identification will then be achieved recursively i.e. through a
#' identification will then be achieved recursively i.e. through a Cholesky
#' Cholesky decomposition of the vcov-matrix if \code{\link{sign_restr}} is
#' \code{NULL}. If set to \code{FALSE}, shocks will be unidentified.
#' @param sign_restr Numeric matrix. Specifies sign restrictions for
#' identification. Elements should be set to \eqn{1} (\eqn{-1}) to restrict for
#' a positive (negative) impact on the respective variables. If no presumption
#' about the impact is available, set corresponding elements to \eqn{0}.
#'
#' @return Returns a named list with options for \code{\link{bvar}}.
#'
#' @examples
#' # Compute impulse responses and FEVDs for 20 time periods with identification
#' # achieved by means of a Cholesky decomposition of the vcov-matrix.
#' bv_irf(horizon = 20, fevd = TRUE)
#'
#' # Compute impulse responses using sign restrictions for identification
#' signs <- matrix(c(1, 0, 1, -1, 1, -1, 1, 1, 0), nrow = 3)
#' bv_irf(sign_restr = signs)
#'
#' # ADD example with real dataset for better understanding!!!
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
