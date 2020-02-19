#' Forecast settings
#'
#' Provide forecast settings to \code{\link{bvar}} or
#' \code{\link{predict.bvar}}. Allows adjusting the horizon of the forecast.
#' Settings for conditional forecasts should be set via \code{\link{bv_cdfcast}}
#' and provided to argument conditional.
#'
#' Conditional forecasts must be retrieved via \code{\link{scenario}}
#' ex-post. Conditional forecasts will require further options, such as
#' variable paths.
#'
#' @param horizon Integer scalar. Horizon for which forecasts should be
#' computed.
#' @param conditional Optional list. Contains list with settings for
#' conditional forecasts created via \code{\link{bv_cdfcast}}.
#'
#' @return Returns a named list of class \code{bv_fcast} with options for
#' \code{\link{bvar}} or \code{\link{predict.bvar}}.
#'
#' @seealso \code{\link{predict.bvar}}; \code{\link{plot.bvar_fcast}};
#' \code{\link{bv_dcfcast}}
#'
#' @keywords VAR BVAR forecasts prediction settings
#'
#' @export
#'
#' @examples
#' # Set the forecast-horizon to 20 time periods instead of 12
#' bv_fcast(horizon = 20)
bv_fcast <- function(
  horizon = 12, conditional = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")


  if(!is.null(conditional)) {
    if(!inherits(conditional, "bv_cdfcast")) {
      stop("Please use `bv_cdfcast()` to configure conditional forecasts.")
    }

    if((is.matrix(conditional[["path"]]) &&
        nrow(conditional[["path"]]) > horizon) ||
       (is.vector(conditional[["path"]]) &&
        length(conditional[["path"]]) > horizon)) {
      stop("Horizon of conditions exceeds forecasting horizon.")
    }
  }


  out <- list("horizon" = horizon, "conditional" = conditional)
  class(out) <- "bv_fcast"

  return(out)
}



# Constructor for conditional forecast settings
bv_cdfcast <- function(path, cond_var = NULL) {

  if(!is.numeric(path)) {stop("Conditions in the wrong format.")}

  if(is.vector(path) && is.null(cond_var)) {
    stop("Position/Name of constrained variable missing.")
  }

  if(is.matrix(path) && !is.null(cond_var) && ncol(path) != length(cond_var)) {
    stop("Number of constrained variables in path and number of positions/names
         do not match")
  }

  if(is.matrix(path) && !all(apply(path, 1, function(x) any(is.na(x))))) {
    stop("All variables restricted at once at some point.")
  }

  out <- list("path" = path, "cond_var" = cond_var)
  class(out) <- "bv_cdfcast"

  return(out)
}

