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
  col = "#737373",
  t_back = 1,
  area = FALSE,
  fill = "#808080",
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}
  plot_fcast(x = x, conf_bands = conf_bands, vars = vars,
    variables = variables, orientation = orientation, mar = mar,
    t_back = t_back, area = area, col = col, fill = fill, ...)
}


#' @noRd
plot_fcast <- function(
  x,
  conf_bands, # deprecated, see `predict.bvar()`
  vars = NULL,
  variables = NULL,
  orientation = c("vertical", "horizontal"),
  mar = c(2, 2, 2, 0.5),
  t_back = 1,
  area = FALSE,
  col = "#737373",
  fill = "#808080",
  ...) {

  # Checks ----------------------------------------------------------------

  if(!inherits(x, "bvar") && !inherits(x, "bvar_fcast")) {
    stop("Please provide a `bvar` or `bvar_fcast` object.")
  }
  if(inherits(x, "bvar")) {x <- predict(x)}

  if(!missing(conf_bands)) {
    message("Parameter conf_bands is deprecated. Please use `predict()`.")
    x <- predict(x, conf_bands = conf_bands)
  }

  t_back <- int_check(t_back, 0, nrow(x[["data"]]),
    msg = "Please check n_back.")
  t_forw <- x[["setup"]][["horizon"]]

  has_quants <- length(dim(x[["quants"]])) == 3L
  if(has_quants) {
    quants <- x[["quants"]]
    M <- dim(quants)[3]
    P <- P2 <- dim(quants)[1]
  } else {
    M <- dim(x[["quants"]])[2]
    P <- 1
    P2 <- 2
    # Cheat day - quants must be 3-dimensional, so we fill with NAs
    quants <- array(NA, c(2, dim(x[["quants"]])))
    quants[1, , ] <- x[["quants"]]
  }

  # Add t_back actual datapoints
  # Make sure it can still be called with earlier objects
  if(is.null(x[["data"]])) {
    message("No data found, filling with NAs. Recalculate with `predict()`.")
    data <- matrix(NA, nrow = t_back, ncol = M)
  } else {data <- tail(x[["data"]], t_back)}

  # Extend the quants array with data, quantiles are set to NA
  quants <- vapply(seq(M), function(i) {
    t(rbind(set_na(data[, i], P2), t(quants[, , i])))
  }, matrix(0, P2, t_back + t_forw), USE.NAMES = FALSE)

  variables <- get_deps(variables = x[["variables"]], M = M)

  orientation <- match.arg(orientation)

  # Sort out colours - applies alpha if they're HEX and need recycling
  col <- set_col(x = "#000000", y = col, P = P)
  if(area) {fill <- set_col(x = integer(), y = fill, P = P)}

  pos <- get_var_set(vars, variables, M)

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
  t_back = NA,
  area = FALSE, fill,
  ...) {

  if(area) {
    excl <- seq(t_back)
    P <- dim(x)[1]
    x_vals <- c(seq(dim(x)[2])[-excl], rev(seq(dim(x)[2])[-excl]))
  }
  mid <- length(col) %/% 2 + 1

  op <- par(mfrow = mfrow, mar = mar, ...)
  for(i in pos) {
    ts.plot(t(as.matrix(x[, , i])),
      col = col, lty = 1, main = paste("Forecast", variables[i]))
    # Fill areas
    if(area) {for(j in seq(P - 1)) {
      polygon(y = c(x[j, -excl, i], rev(x[j + 1, -excl, i])),
        x = x_vals, col = fill[j], border = NA)
    }}
    grid()
    abline(v = t_back + 1, lty = "dashed", col = "gray")
    abline(h = 0, lty = "dashed", col = "gray")
    lines(x[mid, , i], col = col[mid])
  }
  par(op)
}
