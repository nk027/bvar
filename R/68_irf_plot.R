#' Plotting method for Bayesian VAR impulse responses
#'
#' Plotting method for impulse responses obtained from \code{\link{irf.bvar}}.
#' Impulse responses of all or a subset of the available variables can be
#' plotted.
#'
#' @param x A \code{bvar_irf} object, obtained from \code{\link{irf.bvar}}.
#' @param vars_impulse,vars_response Optional numeric or character vector. Used
#' to subset the plot's impulses / responses to certain variables by position
#' or name (must be available). Defaults to \code{NULL}, i.e. all variables.
#' @param col Character vector. Colour(s) of the lines delineating credible
#' intervals. Single values will be recycled if necessary. Recycled HEX color
#' codes are varied in transparency if not provided (e.g. "#737373FF"). Lines
#' can be bypassed by setting this to \code{"transparent"}.
#' @param area Logical scalar. Whether to fill the credible intervals using
#' \code{\link[graphics]{polygon}}.
#' @param fill Character vector. Colour(s) to fill the credible intervals with.
#' See \emph{col} for more information.
#' @param variables Optional character vector. Names of all variables in the
#' object. Used to subset and title. Taken from \code{x$variables} if available.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns \emph{x} invisibly.
#'
#' @seealso \code{\link{bvar}}; \code{\link{irf.bvar}}
#'
#' @keywords BVAR irf fevd analysis plot
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
#'
#' # Store IRFs ex-post
#' irf(x) <- irf(x)
#'
#' # Plot impulse responses for all available variables
#' plot(irf(x))
#'
#' # Subset to impulse variables in positions 2 and 3 via their name
#' plot(irf(x), vars_impulse = c(2, 3))
#'
#' # Subset via position and increase the plotted IRF horizon
#' plot(irf(x, horizon = 20), vars_impulse = c("UNRATE", "FED"))
#'
#' # Adjust confidence bands and subset to one response variables
#' plot(irf(x, conf_bands = 0.25), vars_response = "CPI")
#'
#' # Draw areas inbetween the confidence bands and skip drawing lines
#' plot(irf(x), col = "transparent", area = TRUE)
#'
#' # Subset to a specific impulse and response
#' plot(irf(x), vars_response = "CPI", vars_impulse = "FED")
#' }
plot.bvar_irf <- function(
  x,
  vars_response = NULL,
  vars_impulse = NULL,
  col = "#737373",
  area = FALSE,
  fill = "#808080",
  variables = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}
  plot_irf(x = x,
    vars_response = vars_response, vars_impulse = vars_impulse,
    variables = variables, mar = mar, area = area, col = col, fill = fill, ...)
}


#' @noRd
plot_irf <- function(
  x,
  vars_response = NULL,
  vars_impulse = NULL,
  variables = NULL,
  mar = c(2, 2, 2, 0.5),
  area = FALSE,
  col = "#737373",
  fill = "#808080",
  ...) {

  # Checks ---

  if(!inherits(x, "bvar") && !inherits(x, "bvar_irf")) {
    stop("Please provide a `bvar` or `bvar_irf` object.")
  }
  if(inherits(x, "bvar")) {x <- irf(x)}

  # Prepare data ---

  has_quants <- length(dim(x[["quants"]])) == 4L
  if(has_quants) {
    quants <- x[["quants"]]
    M <- dim(quants)[2]; P <- dim(quants)[1]
  } else {
    M <- dim(x[["quants"]])[1]; P <- 1
    # Cheat day - quants must be 4-dimensional, so we fill with NAs
    quants <- array(NA, c(2, dim(x[["quants"]])))
    quants[1, , , ] <- x[["quants"]]
  }

  # Prepare other arguments ---

  variables <- name_deps(variables = if(is.null(variables)) {
    x[["variables"]]} else {variables}, M = M)

  # Sort out colours - applies alpha if they're HEX and need recycling
  col <- fill_ci_col(x = "#000000", y = col, P = P)
  if(area) {fill <- fill_ci_col(x = integer(), y = fill, P = P)}

  pos_imp <- pos_vars(vars_impulse, variables, M)
  pos_res <- pos_vars(vars_response, variables, M)

  mfrow <- c(length(pos_res), length(pos_imp))

  .plot_irf(x = quants, variables = variables,
    pos_imp = pos_imp, pos_res = pos_res,
    col = col, mar = mar, mfrow = mfrow,
    area = area, fill = fill, ...)

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
#' @param area Logical scalar. Whether to draw polygons between intervals.
#' @param fill Character vector. Colours for \code{\link[graphics]{polygon}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @importFrom graphics par grid abline polygon
#' @importFrom stats ts.plot ts
#'
#' @noRd
.plot_irf <- function(
  x,
  variables,
  pos_imp,
  pos_res,
  col, mar, mfrow,
  area = FALSE, fill,
  ...) {

  if(area) {
    P <- dim(x)[1]
    x_vals <- c(seq(dim(x)[3]), rev(seq(dim(x)[3])))
  }
  mid <- length(col) %/% 2 + 1

  op <- par(mfrow = mfrow, mar = mar, ...)
  for(i in pos_res) {
    for(j in pos_imp) {
       ts.plot(t(as.matrix(x[, i, , j])), col = col, lty = 1,
        main = paste("Shock", variables[j], "on", variables[i]))
      # Fill areas
      if(area) {for(k in seq(P - 1)) {
        polygon(y = c(x[k, i, , j], rev(x[k + 1, i, , j])),
          x = x_vals, col = fill[k], border = NA)
      }}
      grid()
      abline(h = 0, lty = "dashed", col = "gray")
      lines(x[mid, i, , j], col = col[mid])
    }
  }
  par(op)
}
