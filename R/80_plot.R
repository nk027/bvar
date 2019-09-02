#' Plotting method for Bayesian VARs
#'
#' Method to plot trace and densities of hyperparameters and marginal likelihood
#' or coefficient values obtained from \code{\link{bvar}}. Plots may be subset
#' to certain types using \emph{type} and to hyperparameters using \emph{vars}.
#' Multiple chains, i.e. comparable \code{bvar} objects may be plotted together
#' using the \emph{chains} argument.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param type A string with the type of plot desired. The standard method
#' \code{"full"} includes both density and trace plots.
#' @param vars Optional character vector used to subset the plot. The elements
#' need to match the names of hyperparameters (including \code{"ml"}). Defaults
#' to \code{NULL}, i.e. all variables.
#' @param vars_response,vars_impulse Optional integer vectors with the
#' positions of coefficient values to retrieve densities of.
#' \emph{vars_response} corresponds to a specific dependent variable,
#' \emph{vars_impulse} to an independent one. Note that the constant is found
#' at position one.
#' @param chains List with additional \code{bvar} objects. Contents are then
#' added to trace and density plots.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @seealso \code{\link{plot.bvar_fcast}}; \code{\link{plot.bvar_irf}}.
#'
#' @keywords VAR BVAR trace density convergence plot
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#' y <- bvar(data, lags = 2)
#'
#' # Plot full traces and densities
#' plot(x)
#'
#' # Compare with second chain
#' plot(x, chains = y)
#'
#' # Only plot the marginal likelihood's density
#' plot(x, "dens", "ml")
#' }
plot.bvar <- function(
  x,
  type = c("full", "trace", "density"),
  vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  chains = list(),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(x) {if(!inherits(x, "bvar")) {
    stop("Please provide `bvar` objects to the chains parameter.")
  }})

  type <- match.arg(type)


  # Get data and plot -------------------------------------------------------

  prep <- prep_data(x, vars, vars_response, vars_impulse,
                    chains, check_chains = TRUE, n_saves = TRUE)
  data <- prep[["data"]]
  vars <- prep[["vars"]]
  chains <- prep[["chains"]]
  bounds <- prep[["bounds"]]

  plot_bvar(data, type, vars, chains, bounds, mar, ...)

  return(invisible(x))
}


#' @rdname plot.bvar
#'
#' @importFrom graphics par
plot_bvar <- function(
  x,
  type = c("full", "trace", "density"),
  vars = NULL,
  chains = list(),
  bounds = NULL,
  mar = c(2, 2, 2, 0.5),
  ...) {

  # Plot --------------------------------------------------------------------

  op <- par(mfrow = c(length(vars), if(type == "full") {2} else {1}),
            mar = mar, ...)

  for(i in seq_len(ncol(x))) {

    if(type != "density") { # i.e. full or trace
      plot_trace(x[, i], name = vars[i], bounds = bounds[, i],
                 dots = lapply(chains, function(x) {x[, i]}))
    }
    if(type != "trace") { # i.e. full or density
      plot_dens(x[, i], name = vars[i], bounds = bounds[, i],
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
plot_trace <- function(x, name = NULL, bounds = NULL, dots = list()) {

  ylim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))

  plot(x, type = "l", xlab = "Index", ylab = "Value", ylim = ylim,
       main = paste0("Trace", if(!is.null(name)) {paste(" of", name)}))
  for(dot in dots) {lines(dot, col = "lightgray")}
  lines(x)
  abline(h = bounds, lty = "dashed", col = "darkgray")
}


#' @rdname plot_trace
#' @noRd
plot_dens <- function(x, name = NULL, bounds = NULL, dots = list()) {

  xlim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))

  plot(density(x), xlim = xlim,
       main = paste0("Density", if(!is.null(name)) {paste(" of", name)}))
  polygon(density(x), col = "#CCCCCC33", border = NA)
  for(dot in dots) {
    dens <- density(dot)
    polygon(dens, col = "#CCCCCC33", border = NA)
    lines(dens)
  }
  lines(density(x))
  abline(v = bounds, lty = "dashed", col = "darkgray")
}
