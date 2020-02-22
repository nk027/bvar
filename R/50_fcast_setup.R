#' Forecast settings
#'
#' Provide forecast settings to \code{\link{bvar}} or
#' \code{\link{predict.bvar}}. Allows adjusting the horizon of the forecast for
#' unconditional forecasts and settings for conditional forecasts. These should
#' be set via \code{\link{bv_cdfcast}} and provided to argument
#' \emph{conditional}.
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
#' \code{\link{bv_cdfcast}}
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
  cond_path = NULL,
  cond_vars = NULL) {

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


  out <- structure(list(
    "horizon" = horizon, "conditional" = conditional),
    class = "bv_fcast")

  return(out)
}


#' Conditional forecast settings
#'
#' Provide forecast settings to \code{\link{bv_fcast}} or
#' \code{\link{predict.bvar}}. Allows adjustment of settings for conditional
#' forecasts i.e. the conditions imposed on certain variables and their
#' respective name or position in the dataset.
#'
#' @param path Numeric vector or matrix. Contains the path(s) of variable(s)
#' on which forecasts are conditioned on. Unrestricted future realisations
#' should be filled with \code{NA}. Note that not all variables can be
#' restricted at the same time.
#' @param cond_var Optional vector. Containing variable names or positions in
#' case \emph{path} only restricts a subset of the variables.
#'
#' @return Returns a named list of class \code{bv_cdfcast} with options for
#' \code{\link{bv_fcast}} or \code{\link{predict.bvar}}.
#'
#' @seealso \code{\link{predict.bvar}}; \code{\link{plot.bvar_fcast}};
#'
#' @keywords VAR BVAR conditonal forecasts settings
#'
#' @export
#'
#' @examples
#' # Set the path of the second variable for the first six forecast periods.
#' bv_cdfcast(path = c(1, 1, 1, 1, 1, 1), cond_var = 2)
#'
#' # Set the path of the first and third variable at different times.
#' paths <- matrix(NA, nrow = 10, ncol = 2)
#' paths[1:5, 1] <- 1
#' paths[6:10, 2] <- 2
#' bv_cdfcast(path = paths, cond_var = c(1, 3))
#'
#' # Set path for all variables (assuming 3-variable VAR).
#' paths <- matrix(NA, nrow = 10, ncol = 3)
#' paths[1:5, 1] <- 1
#' paths[6:10, 2] <- 2
#' paths[3:8, 3]  <- 3
#' bv_cdfcast(path = paths)
bv_cdfcast <- function(path, cond_var = NULL) {

  if(!is.numeric(path)) {stop("Conditions in the wrong format.")}

  if(is.vector(path) && is.null(cond_var)) {
    stop("Position/Name of constrained variable missing.")
  }

  if(is.matrix(path) && !is.null(cond_var) && ncol(path) != length(cond_var)) {
    stop("Number of constrained variables in path and number of positions/names
         do not match")
  }

  if(!is.null(cond_var) && any(duplicated(cond_var))) {
    stop("Multiple paths given for the same variable. Please adjust cond_var.")
  }

  if(is.matrix(path) && !all(apply(path, 1, function(x) any(is.na(x))))) {
    stop("All variables restricted at once at some point.")
  }

  out <- list("path" = path, "cond_var" = cond_var)
  class(out) <- "bv_cdfcast"

  return(out)
}

