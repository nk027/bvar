
#' Plotting method for Bayesian VAR predictions
#'
#' Plotting method for forecasts obtained from \code{\link{predict.bvar}}.
#' Forecasts of all or a subset of the available variables can be plotted.
#'
#' @param x A \code{bvar_fcast} object, obtained from \code{\link{predict.bvar}}.
#' @param vars Optional numeric or character vector. Used to subset the plot to
#' certain variables by position or name (must be available). Defaults to
#' \code{NULL}, i.e. all variables.
#' @param col Character vector. Colour(s) of the lines delineating credible
#' intervals. Single values will be recycled if necessary. Recycled HEX color
#' codes are varied in transparency if not provided (e.g. "#737373FF"). Lines
#' can be bypassed by setting this to \code{"transparent"}.
#' @param t_back Integer scalar. Number of observed datapoints to plot ahead of
#' the forecast.
#' @param area Logical scalar. Whether to fill the credible intervals using
#' \code{\link[graphics]{polygon}}.
#' @param fill Character vector. Colour(s) to fill the credible intervals with.
#' See \emph{col} for more information.
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
#' @keywords BVAR forecast analysis plot
#'
#' @export
#'
#' @importFrom utils tail
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
#' # Store predictions ex-post
#' predict(x) <- predict(x)
#'
#' # Plot forecasts for all available variables
#' plot(predict(x))
#'
#' # Subset to variables in positions 1 and 3 via their name
#' plot(predict(x), vars = c("CPI", "FED"))
#'
#' # Subset via position, increase the plotted forecast horizon and past data
#' plot(predict(x, horizon = 20), vars = c(1, 3), t_back = 10)
#'
#' # Adjust confidence bands and the plot's orientation
#' plot(predict(x, conf_bands = 0.25), orientation = "h")
#'
#' # Draw areas inbetween the confidence bands and skip drawing lines
#' plot(predict(x), col = "transparent", area = TRUE)
#'
#' # Plot a conditional forecast (with a constrained second variable).
#' plot(predict(x, cond_path = c(1, 1, 1, 1, 1, 1), cond_var = 2))
#' }
plot.bvar_fcast <- function(
  x,
  vars = NULL,
  col = "#737373",
  t_back = 1,
  area = FALSE,
  fill = "#808080",
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}
  plot_fcast(x = x, vars = vars,
    variables = variables, orientation = orientation, mar = mar,
    t_back = t_back, area = area, col = col, fill = fill, ...)
}


#' @noRd
plot_fcast <- function(
  x,
  vars = NULL,
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  t_back = 1,
  area = FALSE,
  col = "#737373",
  fill = "#808080",
  ...) {

  # Checks ---

  if(!inherits(x, "bvar") && !inherits(x, "bvar_fcast")) {
    stop("Please provide a `bvar` or `bvar_fcast` object.")
  }
  if(inherits(x, "bvar")) {x <- predict(x)}

  orientation <- match.arg(orientation)

  # Prepare data ---

  has_quants <- length(dim(x[["quants"]])) == 3L
  if(has_quants) {
    quants <- x[["quants"]]
    M <- dim(quants)[3]; P <- P2 <- dim(quants)[1]
  } else {
    if(area) {message("Cannot plot area without quantiles."); area <- FALSE}
    M <- dim(x[["quants"]])[2]; P <- 1; P2 <- 2
    # Cheat day - quants must be 3-dimensional, so we fill with NAs
    quants <- array(NA, c(2, dim(x[["quants"]])))
    quants[1, , ] <- x[["quants"]]
  }

  # Add t_back actual datapoints
  t_back <- int_check(t_back, 0, Inf, msg = "Issue with t_back.")

  use_data <- t_back != 0
  if(use_data) {
    if(is.null(x[["data"]])) { # To support versions prior to 1.0.0
      message("No data found, filling with NAs. Recalculate with `predict()`.")
      t_back <- 0L
    } else {
      data <- tail(x[["data"]], t_back)
      t_forw <- x[["setup"]][["horizon"]]
    }
    # Extend the quants array with data, quantiles are set to NA
    quants <- vapply(seq(M), function(i) {
      t(rbind(fill_ci_na(data[, i], P2), t(quants[, , i])))
    }, matrix(0, P2, t_back + t_forw), USE.NAMES = FALSE)
  }

  # Prepare other arguments ---

  variables <- name_deps(variables = if(is.null(variables)) {
    x[["variables"]]} else {variables}, M = M)

  # Sort out colours - applies alpha if they're HEX and need recycling
  col <- fill_ci_col(x = "#000000", y = col, P = P)
  if(area) {fill <- fill_ci_col(x = integer(), y = fill, P = P)}

  pos <- pos_vars(vars, variables, M)

  mfrow <- if(grepl("^vertical$", orientation)) {
    c(length(pos), 1)
  } else {c(1, length(pos))}

  .plot_fcast(x = quants, variables = variables,
    pos = pos, col = col, mar = mar, mfrow = mfrow,
    t_back = t_back, area = area, fill = fill, ...)

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
#' @param t_back Integer scalar. Number of initial datapoints without intervals.
#' @param area Logical scalar. Whether to draw polygons between intervals.
#' @param fill Character vector. Colours for \code{\link[graphics]{polygon}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @importFrom graphics par grid abline polygon
#' @importFrom stats ts.plot ts
#'
#' @noRd
.plot_fcast <- function(
  x,
  variables,
  pos,
  col, mar, mfrow,
  t_back = 0,
  area = FALSE, fill,
  ...) {

  if(area) {
    excl <- if(t_back > 0) {seq(t_back)} else {0}
    P <- dim(x)[1]
    x_vals <- c(seq(dim(x)[2] - t_back), rev(seq(dim(x)[2] - t_back)))
  }
  mid <- length(col) %/% 2 + 1

  op <- par(mfrow = mfrow, mar = mar)
  for(i in pos) {
    ts.plot(ts(t(as.matrix(x[, , i])), start = (-t_back + 1)),
      col = col, lty = 1, main = paste("Forecast", variables[i]))
    # Fill areas
    if(area) {for(j in seq(P - 1)) {
      polygon(y = c(x[j, -excl, i], rev(x[j + 1, -excl, i])),
        x = x_vals, col = fill[j], border = NA)
    }}
    grid()
    abline(v = 1, lty = "dashed", col = "gray")
    abline(h = 0, lty = "dashed", col = "gray")
    lines(ts(x[mid, , i], start = (-t_back + 1)), col = col[mid])
  }
  par(op)
}
