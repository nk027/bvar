#' Forecast plot
#'
#' Plot method for forecasts obtained from \code{\link{bvar}}. Plot forecasts
#' for all or a subset of the available variables.
#'
#' @param x A \code{bvar} / \code{bvar_fcast} object, obtained from
#' \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands. For 5%, 10%,
#' 90% and 95% bands set this to \code{c(0.05, 0.1)}.
#' @param variables Optional character vector. Names of all variables in the
#' object. Taken from \code{x$variables} if available.
#' @param vars Optional numeric or character vector. Used to subset the plot to
#' certain variables by position or name (\code{variables} must be available).
#' Defaults to \code{NULL}, i.e. all variables.
#' @param mar Numeric vector with margins for \code{\link[graphics]{par}}.
#' @param orientation String indicating the orientation of the plots. Defaults
#' to "v" (i.e. "vertical"); may be set to "h" (i.e. "horizontal").
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns \code{x} invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # x <- bvar()
#'
#' # Plot forecasts for all available variables
#' bv_plot_fcast(x)
#'
#' # Subset to variables in positions 1, 2 and 4 via position and name
#' bv_plot_fcast(x, vars = c(1, 2, 4))
#' bv_plot_fcast(x,
#'   variables = c("gdp", "flux", "cpi", "capacitor"),
#'   vars = c("gdp", "flux", "capacitor")
#' )
#'
#' # Use the method to plot and adjust confidence bands and orientation
#' plot(x$fcast, conf_bands = c(0.01, 0.05), orientation = "h")
#' }
plot.bvar_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  mar = c(2, 2, 2, 0.5),
  orientation = c("vertical", "horizontal"),
  ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}
  bv_plot_fcast(x, conf_bands, variables, vars, mar, orientation, ...)

}


#' @rdname plot.bvar_fcast
#' @export
bv_plot_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  mar = c(2, 2, 2, 0.5),
  orientation = c("vertical", "horizontal"),
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

  mfrow <- if(grep("^vertical$", orientation)) {
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
#' @param mar Numeric vector with margins for \code{\link[graphics]{par}}.
#' @param mfrow Numeric vector with layout for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
plot_fcast <- function(
  x,
  variables,
  pos,
  col, mar, mfrow, ...) {

  op <- par(mfrow = mfrow, mar = mar, ...)
  for(i in pos) {
    ts.plot(t(as.matrix(x[, , i])),
            col = col, lty = 1,
            main = paste("Forecast", variables[i]))
    grid()
  }
  par(op)
}
