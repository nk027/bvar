
#' Plotting method for Bayesian VARs
#'
#' Method to plot trace and densities of coefficient, hyperparameter and
#' marginal likelihood draws obtained from \code{\link{bvar}}. Several types of
#' plot are available via the argument \emph{type}, including traces, densities,
#' plots of forecasts and impulse responses.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param type A string with the type of plot desired. The default option
#' \code{"full"} plots both densities and traces.
#' @param vars Character vector used to select variables. Elements are matched
#' to hyperparameters or coefficients. Coefficients may be matched based on
#' the dependent variable (by providing the name or position) or the
#' explanatory variables (by providing the name and the desired lag). See the
#' example section for a demonstration. Defaults to \code{NULL}, i.e. all
#' hyperparameters.
#' @param vars_response,vars_impulse Optional character or integer vectors used
#' to select coefficents. Dependent variables are specified with
#' \emph{vars_response}, explanatory ones with \emph{vars_impulse}. See the
#' example section for a demonstration.
#' @param chains List of \code{bvar} objects. Contents are then added to trace
#' and density plots to help assessing covergence.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns \emph{x} invisibly.
#'
#' @seealso \code{\link{bvar}}; \code{\link{plot.bvar_fcast}};
#' \code{\link{plot.bvar_irf}}.
#'
#' @keywords BVAR MCMC plot analysis
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
#' # Plot full traces and densities
#' plot(x)
#'
#' # Only plot the marginal likelihood's trace
#' plot(x, "trace", "ml")
#'
#' # Access IRF and forecast plotting functions
#' plot(x, type = "irf", vars_response = 2)
#' plot(x, type = "fcast", vars = 2)
#' }
plot.bvar <- function(
  x,
  type = c("full", "trace", "density", "irf", "fcast"),
  vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  chains = list(),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar")) {
    if(inherits(x[[1]], "bvar")) { # Allow chains to x
      chains <- x
      x <- x[[1]]
      chains[[1]] <- NULL
    } else {stop("Please provide a `bvar` object.")}
  }

  type <- match.arg(type)

  # Forward and return if "irf" or "fcast"
  if(type == "irf") {
    if(is.null(x[["irf"]])) {message("No `bvar_irf` found. Calculating...")}
    return(plot.bvar_irf(
      irf(x), vars_response = vars_response, vars_impulse = vars_impulse,
      variables = x[["variables"]], mar = mar, ...))
  }
  if(type == "fcast") {
    if(is.null(x[["fcast"]])) {message("No `bvar_fcast` found. Calculating...")}
    return(plot.bvar_fcast(
      predict(x), vars = vars, variables = x[["variables"]], mar = mar, ...))
  }

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(x) {if(!inherits(x, "bvar")) {
    stop("Please provide `bvar` objects to the chains parameter.")
  }})


  # Get data and plot -------------------------------------------------------

  prep <- prep_data(x,
    vars = vars, vars_response = vars_response, vars_impulse = vars_impulse,
    chains, check_chains = FALSE)

  .plot_bvar(prep[["data"]], type,
    prep[["vars"]], prep[["chains"]], prep[["bounds"]], mar, ...)

  return(invisible(x))
}


#' @export
plot.bvar_chains <- plot.bvar


#' @rdname .plot_trace
#' @noRd
#'
#' @importFrom graphics par
.plot_bvar <- function(
  x,
  type = c("full", "trace", "density"),
  vars = NULL,
  chains = list(),
  bounds = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  # Plot ---

  op <- par(mfrow = c(length(vars), if(type == "full") {2} else {1}),
    mar = mar, ...)

  for(i in seq_len(ncol(x))) {

    if(type != "density") { # i.e. full or trace
      .plot_trace(x[, i], name = vars[i], bounds = bounds[, i],
        dots = lapply(chains, function(x) {x[, i]}))
    }
    if(type != "trace") { # i.e. full or density
      .plot_dens(x[, i], name = vars[i], bounds = bounds[, i],
        dots = lapply(chains, function(x) {x[, i]}))
    }
  }

  par(op)

  return(invisible(x))
}


#' Trace & density plot
#'
#' @param x Numeric vector to plot.
#' @param name Optional string with the plotted parameter's name.
#' @param bounds Optional numeric vector plotted horizontally via
#' \code{\link[graphics]{abline}}.
#' @param dots Optional list of numeric vectors to add to the plot.
#'
#' @importFrom graphics plot polygon lines abline
#' @importFrom stats density
#'
#' @noRd
.plot_trace <- function(x, name = NULL, bounds = NULL, dots = list()) {

  ylim <- c(min(vapply(dots, min, double(1)), x),
    max(vapply(dots, max, double(1)), x))

  plot(x, type = "l", xlab = "Index", ylab = "Value", ylim = ylim,
    main = paste0("Trace", if(!is.null(name)) {paste(" of", name)}))
  for(dot in dots) {lines(dot, col = "lightgray")}
  lines(x)
  abline(h = bounds, lty = "dashed", col = "darkgray")

  return(invisible(x))
}


#' @rdname .plot_trace
#' @noRd
.plot_dens <- function(x, name = NULL, bounds = NULL, dots = list()) {

  xlim <- c(min(vapply(dots, min, double(1)), x),
    max(vapply(dots, max, double(1)), x))
  ylim <- c(0, max(vapply(lapply(dots, function(x) density(x)[["y"]]),
    max, double(1)), density(x)[["y"]]))

  plot(density(x), xlim = xlim, ylim = ylim,
    main = paste0("Density", if(!is.null(name)) {paste(" of", name)}))
  polygon(density(x), col = "#CCCCCC33", border = NA)
  for(dot in dots) {
    dens <- density(dot)
    polygon(dens, col = "#CCCCCC33", border = NA)
    lines(dens)
  }
  lines(density(x))
  abline(v = bounds, lty = "dashed", col = "darkgray")

  return(invisible(x))
}
