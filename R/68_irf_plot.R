#' Plotting method for Bayesian VAR impulse responses
#'
#' Plotting method for impulse responses obtained from \code{\link{bvar}} or
#' \code{\link{irf.bvar}}. Impulse responses of all or a subset of the
#' available variables can be plotted.
#'
#' @param x A \code{bvar} / \code{bvar_irf} object, obtained from
#' \code{\link{bvar}} / \code{\link{irf.bvar}}.
#' @param conf_bands Deprecated. Use \code{\link{irf.bvar}}. Numeric vector
#' of desired confidence bands.
#' @param vars_impulse,vars_response Optional numeric or character vector. Used
#' to subset the plot's impulses / responses to certain variables by position
#' or name (must be available). Defaults to \code{NULL}, i.e. all variables.
#' @param variables Optional character vector. Names of all variables in the
#' object. Used to subset and title. Taken from \code{x$variables} if available.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns \emph{x} invisibly.
#'
#' @seealso \code{\link{bvar}}; \code{\link{irf.bvar}}
#'
#' @keywords VAR BVAR irf fevd plot
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(400), ncol = 4)
#' x <- bvar(data, lags = 2, irf = bv_irf())
#'
#' # Plot impulse responses for all available variables
#' plot(irf(x))
#' # Alternatively
#' plot(x$irf)
#'
#' # Subset to impulse variables in positions 2 and 4 via position and name
#' plot(x$irf, vars_impulse = c(2, 4))
#' plot(x$irf,
#'   variables = c("solved", "for", "many", "decades"),
#'   vars_impulse = c("for", "decades")
#' )
#'
#' # Adjust confidence bands via irf
#' plot(irf(x, conf_bands = c(0.01, 0.05)))
#' }
plot.bvar_irf <- function(
  x,
  conf_bands, # deprecated, see `irf.bvar()`
  vars_response = NULL,
  vars_impulse = NULL,
  variables = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}
  plot_irf(x, conf_bands, vars_response, vars_impulse, variables, mar, ...)
}


#' @noRd
plot_irf <- function(
  x,
  conf_bands, # deprecated, see `irf.bvar()`
  vars_response = NULL,
  vars_impulse = NULL,
  variables = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar") && !inherits(x, "bvar_irf")) {
    stop("Please provide a `bvar` or `bvar_irf` object.")
  }
  if(inherits(x, "bvar")) {x <- irf(x)}

  if(!missing(conf_bands)) {
    message("Parameter conf_bands is deprecated. Please use `irf()`.")
    x <- irf(x, conf_bands = conf_bands)
  }

  has_quants <- length(dim(x[["quants"]])) == 4L
  if(has_quants) {
    M <- dim(x[["quants"]])[2]
    P <- dim(x[["quants"]])[1]
    quants <- x[["quants"]]
  } else {
    M <- dim(x[["quants"]])[1]
    P <- 1
    # Cheat day - quants must be 4-dimensional, so we fill one with NAs
    quants <- array(NA, c(2, dim(x[["quants"]])))
    quants[1, , , ] <- x[["quants"]]
  }

  if(is.null(variables)) {
    variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
  } else if(length(variables) != M) {stop("Vector variables is incomplete.")}

  col <- set_gray(P)
  pos_imp <- get_var_set(vars_impulse, variables, M)
  pos_res <- get_var_set(vars_response, variables, M)

  mfrow <- c(length(pos_res), length(pos_imp))

  .plot_irf(quants, variables, pos_imp, pos_res, col, mar, mfrow, ...)

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
.plot_irf <- function(
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
