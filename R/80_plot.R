#' Hyperparameter plot
#'
#' A collection of diagnostic plots with traces and densities of the marginal
#' posterior likelihood and available hyperparameters. Minimum and maximum
#' values of the hyperparameters are plotted as darkgray dashed lines.
#'
#' @param x An \code{bvar}, obtained from \code{\link{bvar}}.
#' @param mar Numeric vector with margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns x invisibly.
#' @export
plot.bvar <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object with class bvar.")}

  bv_plot(x, ...)

}
