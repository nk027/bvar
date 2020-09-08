#' Impulse response settings and identification
#'
#' Provides settings for the computation of impulse responses to
#' \code{\link{bvar}}, \code{\link{irf.bvar}} or \code{\link{fevd.bvar}}. Allows
#' setting the horizon for which impulse responses should be computed, whether
#' or not forecast error variance decompositions (FEVDs) should be included
#' and if and what kind of identification should be used.
#'
#' Identification can be performed via Cholesky decomposition, sign
#' restrictions as well as zero and sign restrictions. The algorithm
#' for generating suitable sign restrictions follows Rubio-Ramirez, Waggoner
#' and Zha (2010), while the one for zero and sign restrictions follows
#' Arias, Rubio-Ramirez and Waggoner (2018).
#' Note the possiblity of finding no suitable zero/sign restrictions.
#'
#' @param horizon Integer scalar. The horizon for which impulse responses
#' (and FEVDs) should be computed. Note that the first period corresponds to
#' impacts i.e. contemporaneous effects.
#' @param fevd Logical scalar. Whether or not forecast error variance
#' decompositions should be calculated.
#' @param identification Logical scalar. Whether or not the shocks used for
#' calculating impulses should be identified. Defaults to \code{TRUE}, i.e.
#' identification via Cholesky decomposition of the VCOV-matrix unless
#' \emph{sign_restr} or \emph{zero_restr} is provided.
#' @param sign_restr Numeric matrix. Sign restrictions for identification.
#' Elements should be set to 1 (-1) to restrict for positive
#' (negative) impacts. If no presumption about the impact can be made the
#' corresponding elements can be set to \code{NA}. The default value is
#' \code{NULL}. Note that in order to be fully identified at least
#' \eqn{M * (M - 1) / 2} restrictions have to be set.
#' @param zero_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1}, \eqn{-1} or \eqn{0} depending
#' on whether a positive, a negative or no contemporaneous effect of a
#' certain shock is expected. Elements set to \eqn{NA} indicate that there are
#' no particular expectations for the contemporaneous effects. The default
#' value is \code{NULL}. Note that at most \eqn{M - j} zero restrictions can
#' be set for the \eqn{j}-th column corresponding to a shock of variable
#' \eqn{j}.
#' @param sign_lim Integer scalar. Maximum number of tries to find suitable
#' matrices to for fitting sign or zero and sign restrictions.
#'
#' @return Returns a named list of class \code{bv_irf} with options for
#' \code{\link{bvar}}, \code{\link{irf.bvar}} or \code{\link{fevd.bvar}}.
#'
#' @references
#'   Rubio-Ramirez, J. F. and Waggoner, D. F. and Zha, T. (2010) Structural
#'   Vector Autoregressions: Theory of Identification and Algorithms for
#'   Inference. \emph{The Review of Economic Studies}, \bold{77}, 665-696,
#'   \url{https://doi.org/10.1111/j.1467-937X.2009.00578.x}.
#'   Arias, J.E. and Rubio-Ramirez, J. F. and Waggoner, D. F. (2018)
#'   Inference Based on Structural Vector Autoregressions Identifiied with
#'   Sign and Zero Restrictions: Theory and Applications.
#'   \emph{Econometrica}, \bold{86}, 2, 685-720,
#'   \url{https://doi.org/10.3982/ECTA14468}.
#'
#' @seealso \code{\link{irf.bvar}}; \code{\link{plot.bvar_irf}}
#'
#' @keywords BVAR irf fevd settings
#'
#' @export
#'
#' @examples
#' # Set impulse responses to a horizon of 20 time periods and enable FEVD
#' # (Identification is performed via Cholesky decomposition)
#' bv_irf(horizon = 20, fevd = TRUE)
#'
#' # Set up structural impulse responses using sign restrictions
#' signs <- matrix(c(1, NA, NA, -1, 1, -1, -1, 1, 1), nrow = 3)
#' bv_irf(sign_restr = signs)
#'
#' # Set up structural impulse responses using zero and sign restrictions
#' zero_signs <- matrix(c(1, 0, NA, -1, 1, 0, -1, 1, 1), nrow = 3)
#' bv_irf(zero_restr = zero_signs)
#'
#' # Prepare to estimate unidentified impulse responses
#' bv_irf(identification = FALSE)
bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL,
  zero_restr = NULL,
  sign_lim = 1000) {

  # Input checks
  horizon <- int_check(horizon, min = 1, max = 1e6,
    msg = "Invalid value for horizon (outside of [1, 1e6]).")
  sign_lim <- int_check(sign_lim, min = 1, max = Inf,
    msg = "Invalid value for sign_lim (outside of [1, Inf]).")

  if(!is.logical(c(identification, fevd))){
    stop("Please provide fevd and identification as logical scalars.")
  }

  if(identification) {
    if(!is.null(zero_restr)) {
      restr_len <- length(zero_restr)
      if(!is.numeric(zero_restr) && !all(zero_restr %in% c(-1, 0, NA, 1)) &&
         sqrt(restr_len) %% 1 != 0) {
        stop("Please provide zero_restr as a numeric square matrix ",
             "containing NAs, 0s, 1s and -1s.")
      }
      if(is.vector(zero_restr)) {
        zero_restr <- matrix(zero_restr, nrow = sqrt(restr_len))
      }
      if(any(colSums(signs == 0, na.rm = TRUE) >
             rev(seq_len(sqrt(restr_len)) - 1))) {
        stop("Number of zero restrictions for at least one of the shocks ",
             "too high. Please provide less zero restrictions or change ",
             "order of variables.")
      }
      if(sum(!is.na(zero_restr)) <
         (sqrt(restr_len) - 1) * sqrt(restr_len) / 2) {
        message("Number of restrictions implies an underidentified system.")
      }
    }
    if(!is.null(sign_restr)) {
      restr_len <- length(sign_restr)
      if(!is.numeric(sign_restr) && !all(sign_restr %in% c(-1, 0, NA, 1)) &&
        sqrt(restr_len) %% 1 != 0) {
        stop("Please provide sign_restr as a numeric square matrix ",
          "containing NAs, 1s and -1s.")
      }
      if(0 %in% sign_restr) {
        warning("Please set unrestricted elements to NA instead of 0 ",
          "or use zero_restr for zero and sign restrictions.")
        sign_restr[sign_restr == 0] <- NA_integer_
      }
      if(is.vector(sign_restr)) {
        sign_restr <- matrix(sign_restr, nrow = sqrt(restr_len))
      }
      if(sum(!is.na(sign_restr)) <
        (sqrt(restr_len) - 1) * sqrt(restr_len) / 2) {
        message("Number of restrictions implies an underidentified system.")
      }
    }
    # Cholesky
  }

  # Outputs
  out <- list("horizon" = horizon, "fevd" = fevd,
    "identification" = identification,
    "sign_restr" = sign_restr, "zero_restr" = zero_restr,
    "sign_lim" = sign_lim
  )

  class(out) <- "bv_irf"

  return(out)
}
