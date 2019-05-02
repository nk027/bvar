#' Hyperparameter plot
#'
#' A collection of diagnostic plots with traces and densities of the marginal
#' posterior likelihood and available hyperparameters. Minimum and maximum
#' values of the hyperparameters are plotted as dashed lines in "darkgray".
#'
#' @param x A \code{bvar}, obtained from \code{\link{bvar}}.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @seealso \code{\link{bv_plot_trace}}; \code{\link{bv_plot_irf}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Plot various output generated
#' plot(x)
#' }
plot.bvar <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object with class bvar.")}

  bv_plot(x, ...)

}


#' @export
#' @rdname plot.bvar
#'
#' @importFrom graphics par
bv_plot <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  y <- x[["hyper"]]
  K <- ncol(y)
  name <- colnames(y)
  bounds <- vapply(name, function(z) {
    c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])
  }, double(2))

  op <- par(mfrow = c(K + 1, 2), mar = mar, ...)

  plot_trace(x[["ml"]], name = "marginal likelihood")
  plot_dens(x[["ml"]], name = "marginal likelihood")
  for(i in 1:K) {
    plot_trace(y[, i], name[i], bounds[, i])
    plot_dens(y[, i], name[i], bounds[, i])
  }

  par(op)

  return(invisible(x))
}
