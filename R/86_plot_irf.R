#' Impulse response plot
#'
#' Plotting method for impulse responses obtained from \code{\link{bvar}}.
#' Impulse responses of all or a subset of the available variables can be
#' plotted.
#'
#' @param x A \code{bvar} / \code{bvar_irf} object, obtained from
#' \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands. E.g. for bands
#' at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' @param variables Optional character vector. Names of all variables in the
#' object. Taken from \code{x$variables} if available.
#' @param vars_impulse Optional numeric or character vector. Used to subset the
#' plot's impulses to certain variables by position or name (\code{variables}
#' must be available). Defaults to \code{NULL}, i.e. all variables.
#' @param vars_response Optional numeric or character vector. Used to subset the
#' plot's responses to certain variables by position or name (\code{variables}
#' must be available). Defaults to \code{NULL}, i.e. all variables.
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
#' # Plot impulse responses for all available variables
#' bv_plot_irf(x)
#'
#' # Subset to impulse variables in positions 2 and 4 via position and name
#' bv_plot_irf(x, vars_impulse = c(2, 4))
# bv_plot_irf(x,
#   variables = c("solved", "for", "many", "decades"),
#   vars_impulse = c("for", "decades")
# )
#'
#' # Use the method to plot and adjust confidence bands
#' plot(x$irf, conf_bands = c(0.01, 0.05))
#' }
plot.bvar_irf <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars_impulse = NULL,
  vars_response = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}
  bv_plot_irf(x, conf_bands, variables, vars_impulse, vars_response, mar, ...)

}


#' @rdname plot.bvar_irf
#' @export
#'
#' @importFrom stats quantile
bv_plot_irf <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars_impulse = NULL,
  vars_response = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  quantiles <- quantile_check(conf_bands)

  if(inherits(x, "bvar")) {
    y <- apply(x[["irf"]][["irf"]], c(2, 3, 4), quantile, quantiles)
    M <- dim(y)[2]
    P <- dim(y)[1]
    if(is.null(variables)) {
      variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
    }

  } else if(inherits(x, "bvar_irf")) {
    y <- apply(x[["irf"]], c(2, 3, 4), quantile, quantiles)
    M <- dim(y)[2]
    P <- dim(y)[1]
    if(is.null(variables)) {variables <- 1:M}

  } else {stop("Please provide a `bvar` or `bvar_fcast` object.")}

  if(length(variables) != M) {stop("Named vector `variables` is incomplete.")}

  col <- set_gray(P)
  pos_imp <- get_var_set(vars_impulse, variables, M)
  pos_res <- get_var_set(vars_response, variables, M)

  mfrow <- c(length(pos_res), length(pos_imp))

  plot_irf(y, variables, pos_imp, pos_res, col, mar, mfrow, ...)

  return(invisible(x))
}


#' Impulse response plot
#'
#' @param x Numeric array (4-dimensional) with data to plot. The first
#' dimension contains quantiles, the second responses, the third paths and the
#' fourth impulses.
#' @param variables Character vector with the names of variables.
#' @param pos_imp Integer vector. Positions of the impulse variables to plot.
#' @param pos_res Integer vector. Positions of the response variables to plot.
#' @param col Character vector. Colours to feed to \code{\link[stats]{ts.plot}}.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param mfrow Numeric vector. Layout for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @importFrom graphics par grid abline
#' @importFrom stats ts.plot
#'
#' @noRd
plot_irf <- function(
  x,
  variables,
  pos_imp,
  pos_res,
  col, mar, mfrow,
  ...) {

  op <- par(mfrow = mfrow, mar = mar, ...)
  for(i in pos_res) {
    for(j in pos_imp) {
      ts.plot(t(as.matrix(x[, i, , j])),
              col = col, lty = 1,
              main = paste("Shock", variables[j], "on", variables[i]))
      abline(h = 0, lty = "dashed", col = "black")
      grid()
    }
  }
  par(op)
}
