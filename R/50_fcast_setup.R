#' Forecast settings
#'
#' Provide forecast settings to \code{\link{bvar}} or
#' \code{\link{predict.bvar}}. Allows adjusting the horizon of the forecast.
#'
#' As of now only unconditional forecasts are implemented and conditional must
#' be set to \code{FALSE}. Conditional forecasts will require further options,
#' such as variable paths.
#'
#' @param horizon Integer scalar. Horizon for which forecasts should be
#' computed.
#' @param conditional Logical scalar. Not yet implemented.
#' @param x Object of class \code{bv_fcast}.
#' @param ... Not used.
#'
#' @return Returns a named list of class \code{bv_fcast} with options for
#' \code{\link{bvar}} or \code{\link{predict.bvar}}.
#'
#' @seealso \code{\link{bvar}}; \code{\link{predict.bvar_fcast}};
#' \code{\link{plot.bvar_fcast}};
#'
#' @keywords VAR BVAR forecasts prediction settings
#'
#' @export
#'
#' @examples
#' # Set the forecast-horizon to 20 time periods instead of 12
#' bv_fcast(horizon = 20)
bv_fcast <- function(
  horizon = 12,
  conditional = FALSE) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(conditional) {stop("Conditional forecasts not yet implemented.")}

  out <- list("horizon" = horizon, "conditional" = conditional)
  class(out) <- "bv_fcast"

  return(out)
}
