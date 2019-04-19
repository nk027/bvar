#' Impulse response constructor
#'
#' Constructs object to be passed on to `bvar()` for computation of impulse
#' responses
#'
#' @param horizon Integer scalar. Specifies the horizon for which impulse
#' responses should be computed.
#' @param fevd Logical scalar. Specifies whether or not forecast error variance
#' decompositions should be calculated.
#' @param identification Logical scalar. Specifies whether or not the shocks
#' used for calculating the impulse should be identified. Default is TRUE,
#' identification will then be achieved recursively i.e. through a Cholesky
#' decomposition of the vcov-matrix if \emph{sign_restr} is NULL. If set to
#' FALSE, shocks will be unidentified.
#' @param sign_restr Numeric matrix. Specifies sign restrictions for
#' identification. Elements should be set to \eqn{1} (\eqn{-1}) to restrict for
#' a positive (negative) impact on the respective variables. If no presumption
#' about the impact is available, set corresponding elements to \eqn{0}.
#'
#' @return Returns a list of parameters as an object of class "bv_irf" that can
#' be passed on to function `bvar()`.

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
