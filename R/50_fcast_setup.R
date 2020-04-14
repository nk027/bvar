#' Forecast settings
#'
#' Provide forecast settings to \code{\link{predict.bvar}}. Allows adjusting
#' the horizon of forecasts, and for setting up conditional forecasts. See the
#' details section for further information.
#'
#' Conditional forecasts are calculated using the algorithm by Waggoner and Zha
#' (1999). They are set up by imposing a path on selected variables.
#'
#' @param horizon Integer scalar. Horizon for which to compute forecasts.
#' @param cond_path Optional numeric vector or matrix used for conditional
#' forecasts. Supply variable path(s) on which forecasts are conditioned on.
#' Unrestricted future realisations should be filled with \code{NA}. Note that
#' not all variables can be restricted at the same time.
#' @param cond_vars Optional character or numeric vector. Used to subset
#' \emph{cond_path} to specific variable(s) via name or position. Not
#' needed when \emph{cond_path} is constructed for all variables.
#'
#' @return Returns a named list of class \code{bv_fcast} with options for
#' \code{\link{bvar}} or \code{\link{predict.bvar}}.
#'
#' @references
#'   Waggoner, D. F., & Zha, T. (1999). Conditional Forecasts in Dynamic
#'   Multivariate Models. \emph{Review of Economics and Statistics},
#'   \bold{81:4}, 639-651, \url{https://doi.org/10.1162/003465399558508}.
#'
#' @seealso \code{\link{predict.bvar}}; \code{\link{plot.bvar_fcast}}
#'
#' @keywords BVAR forecast settings
#'
#' @export
#'
#' @examples
#' # Set forecast-horizon to 20 time periods for unconditional forecasts
#' bv_fcast(horizon = 20)
#'
#' # Define a path for the second variable (in the initial six periods).
#' bv_fcast(cond_path = c(1, 1, 1, 1, 1, 1), cond_var = 2)
#'
#' # Constrain the paths of the first and third variables.
#' paths <- matrix(NA, nrow = 10, ncol = 2)
#' paths[1:5, 1] <- 1
#' paths[1:10, 2] <- 2
#' bv_fcast(cond_path = paths, cond_var = c(1, 3))
bv_fcast <- function(
  horizon = 12,
  cond_path = NULL,
  cond_vars = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
    msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(!is.null(cond_path)) {
    if(!is.numeric(cond_path)) {stop("Invalid type of cond_path.")}

    if(is.vector(cond_path)) {
      if(is.null(cond_vars)) {
        stop("Please provide the constrained variable(s) via cond_vars.")
      }
      cond_path <- matrix(cond_path)
    }

    if(!is.null(cond_vars)) {
      if(!is.null(cond_vars) && ncol(cond_path) != length(cond_vars)) {
        stop("Dimensions of cond_path and cond_vars do not match.")
      }
      if(any(duplicated(cond_vars))) {
        stop("Duplicated value(s) found in cond_vars.")
      }
    }

    if(nrow(cond_path) > horizon) {
      horizon <- nrow(cond_path)
      message("Increasing horizon to the length of cond_path.")
    }
  }

  out <- structure(list(
    "horizon" = horizon, "cond_path" = cond_path, "cond_vars" = cond_vars),
    class = "bv_fcast")

  return(out)
}
