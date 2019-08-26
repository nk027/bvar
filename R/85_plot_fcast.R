#' Forecast plot
#'
#' Plotting method for forecasts obtained from \code{\link{bvar}}. Forecasts
#' of all or a subset of the available variables can be plotted.
#'
#' @param x A \code{bvar} / \code{bvar_fcast} object, obtained from
#' \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands. E.g. for bands
#' at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' @param variables Optional character vector. Names of all variables in the
#' object. Taken from \code{x$variables} if available.
#' @param vars Optional numeric or character vector. Used to subset the plot to
#' certain variables by position or name (\code{variables} must be available).
#' Defaults to \code{NULL}, i.e. all variables.
#' @param orientation String indicating the orientation of the plots. Defaults
#' to \code{"v"} (i.e. vertical); may be set to \code{"h"} (i.e. horizontal).
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(400), ncol = 4)
#' x <- bvar(data, lags = 2)
#'
#' # Plot forecasts for all available variables
#' bv_plot_fcast(x)
#'
#' # Subset to variables in positions 1, 2 and 4 via position and name
#' bv_plot_fcast(x, vars = c(1, 2, 4))
# bv_plot_fcast(x,
#   variables = c("gdp", "flux", "cpi", "capacitor"),
#   vars = c("gdp", "flux", "capacitor")
# )
#'
#' # Use the method to plot, adjust confidence bands and orientation
#' plot(x$fcast, conf_bands = c(0.01, 0.05), orientation = "h")
#' }
plot.bvar_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}
  bv_plot_fcast(x, conf_bands, variables, vars, orientation, mar, ...)

}


#' @rdname plot.bvar_fcast
#' @export
#'
#' @importFrom stats quantile
bv_plot_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  quantiles <- quantile_check(conf_bands)

  if(inherits(x, "bvar")) {
    y <- apply(x[["fcast"]][["fcast"]], c(2, 3), quantile, quantiles)
    M <- dim(y)[3]
    P <- dim(y)[1]
    if(is.null(variables)) {
      variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
    }

  } else if(inherits(x, "bvar_fcast")) {
    y <- apply(x[["fcast"]], c(2, 3), quantile, quantiles)
    M <- dim(y)[3]
    P <- dim(y)[1]
    if(is.null(variables)) {variables <- 1:M}

  } else {stop("Please provide a `bvar` or `bvar_fcast` object.")}

  if(length(variables) != M) {stop("Named vector `variables` is incomplete.")}

  orientation <- match.arg(orientation)

  col <- set_gray(P)
  pos <- get_var_set(vars, variables, M)

  mfrow <- if(grepl("^vertical$", orientation)) {
    c(length(pos), 1)
  } else {c(1, length(pos))}

  plot_fcast(y, variables, pos, col, mar, mfrow, ...)

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
plot_fcast <- function(
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
