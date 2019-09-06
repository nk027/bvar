#' Plotting method for Bayesian VAR forecasts
#'
#' Plotting method for forecasts obtained from \code{\link{bvar}} or
#' \code{\link{predict.bvar}}. Forecasts of all or a subset of the available
#' variables can be plotted.
#'
#' @param x A \code{bvar} / \code{bvar_fcast} object, obtained from
#' \code{\link{bvar}} / \code{\link{predict.bvar}}.
#' @param conf_bands Deprecated. Use \code{\link{predict.bvar}}. Numeric vector
#' of desired confidence bands.
#' @param vars Optional numeric or character vector. Used to subset the plot to
#' certain variables by position or name (must be available). Defaults to
#' \code{NULL}, i.e. all variables.
#' @param variables Optional character vector. Names of all variables in the
#' object. Used to subset and title. Taken from \code{x$variables} if available.
#' @param orientation String indicating the orientation of the plots. Defaults
#' to \code{"v"} (i.e. vertical); may be set to \code{"h"} (i.e. horizontal).
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns \emph{x} invisibly.
#'
#' @seealso \code{\link{bvar}}; \code{\link{predict.bvar}}
#'
#' @keywords VAR BVAR forecasts prediction plot
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(400), ncol = 4)
#' x <- bvar(data, lags = 2, fcast = bv_fcast())
#'
#' # Plot forecasts for all available variables
#' plot(predict(x))
#' # Alternatively
#' plot(x$fcast)
#'
#' # Subset to variables in positions 1, 2 and 4 via position and name
#' plot(x$fcast, vars = c(1, 2, 4))
#' plot(x$fcast,
#'   variables = c("gdp", "flux", "cpi", "capacitor"),
#'   vars = c("gdp", "flux", "capacitor")
#' )
#'
#' # Use the method to plot and adjust orientation
#' plot(x$fcast, orientation = "h")
#'
#' # Adjust confidence bands via predict
#' plot(predict(x, conf_bands = c(0.01, 0.05)))
#' }
plot.bvar_fcast <- function(
  x,
  conf_bands, # deprecated, see `predict.bvar()`
  vars = NULL,
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}
  plot_fcast(x, conf_bands, vars, variables, orientation, mar, ...)
}


#' @noRd
plot_fcast <- function(
  x,
  conf_bands, # deprecated, see `predict.bvar()`
  vars = NULL,
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar") && !inherits(x, "bvar_fcast")) {
    stop("Please provide a `bvar` or `bvar_fcast` object.")
  }
  if(inherits(x, "bvar")) {x <- predict(x)}

  if(!missing(conf_bands)) {
    message("Parameter conf_bands is deprecated. Please use `predict()`.")
    x <- predict(x, conf_bands = conf_bands)
  }

  has_quants <- length(dim(x[["quants"]])) == 3L
  if(has_quants) {
    quants <- x[["quants"]]
    M <- dim(quants)[3]
    P <- dim(quants)[1]
  } else {
    M <- dim(x[["quants"]])[2]
    P <- 1
    # Cheat day - quants must be 3-dimensional, so we fill one with NAs
    quants <- array(NA, c(2, dim(x[["quants"]])))
    quants[1, , ] <- x[["quants"]]
  }

  if(is.null(variables)) {
    variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
  } else if(length(variables) != M) {stop("Vector variables is incomplete.")}

  orientation <- match.arg(orientation)

  col <- set_gray(P)
  pos <- get_var_set(vars, variables, M)

  mfrow <- if(grepl("^vertical$", orientation)) {
    c(length(pos), 1)
  } else {c(1, length(pos))}

  .plot_fcast(quants, variables, pos, col, mar, mfrow, ...)

  return(invisible(x))
}


#' Forecast plot
#'
#' @param x Numeric array (3-dimensional) with data to plot. The first
#' dimension contains quantiles, the seconds paths and the third variables.
#' @param variables Character vector with the names of variables.
#' @param pos Integer vector. Positions of the variables to plot.
#' @param col Character vector. Colours to feed to \code{\link[stats]{ts.plot}}.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param mfrow Numeric vector. Layout for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @importFrom graphics par grid abline
#' @importFrom stats ts.plot
#'
#' @noRd
.plot_fcast <- function(
  x,
  variables,
  pos,
  col, mar, mfrow,
  ...) {

  op <- par(mfrow = mfrow, mar = mar, ...)
  for(i in pos) {
    ts.plot(t(as.matrix(x[, , i])),
            col = col, lty = 1,
            main = paste("Forecast", variables[i]))
    abline(h = 0, lty = "dashed", col = "black")
    grid()
  }
  par(op)
}
