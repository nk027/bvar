#' Forecast settings
#'
#' Function to provide settings regarding forecasts to \code{\link{bvar}}.
#' Provides an option to set the forecasting horizon. As of now only
#' unconditional forecasts are implemented, hence the default setting of option
#' \emph{conditional} is \code{NULL}. Once conditional forecasts are implemented as,
#' further options regarding the paths to condition on of certain variables will
#' have to be provided as well.
#'
#' If turned on, forecasts will be drawn recursively for the horizon provided
#' from their posterior distribution using the posterior draws of the VAR
#' coefficients and the corresponding vcov-matrix sigma_draw.
#'
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed. Defaults to 12.
#' @param conditional List containing the conditioning path of certain
#' variables and their name or position in the dataset. Currently not
#' implemented, thus not functionable.
#'
#' @return Returns a named list with options for \code{\link{bvar}}.
#' @export
#'
#' @examples
#' # Compute forecasts for a horizon of 20 time periods (default setting is 12)
#' bv_fcast(horizon = 20)
bv_fcast <- function(
  horizon = 12,
  conditional = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(!is.null(conditional)) stop("Conditional forecasts not yet implemented.")

  out <- list("horizon" = horizon, "conditional" = conditional)
  class(out) <- "bv_fcast"

  return(out)
}
