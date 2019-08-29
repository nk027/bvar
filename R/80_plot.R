#' Plotting method for Bayesian VARs
#'
#' Method to plot trace and densities of hyperparameters and marginal likelihood
#' obtained from \code{\link{bvar}}. Plots may be subset to certain types using
#' \emph{type} and to hyperparamters using \emph{vars}.
#' Multiple chains, i.e. comparable \code{bvar} objects may be plotted together
#' using the \emph{chains} argument.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param type A string with the type of plot desired. The standard method
#' \code{"full"} includes both density and trace plots.
#' @param vars Optional character vector used to subset the plot. The elements
#' need to match the names of hyperparameters (including \code{"ml"}). Defaults
#' to \code{NULL}, i.e. all variables.
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
#' }
plot.bvar <- function(
  x,
  type = c("full", "trace", "density"),
  vars = NULL,
  chains = list(),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  type <- match.arg(type)

  switch(
    type,
    full = bv_plot(x, type, vars, chains, mar, ...),
    trace = bv_plot(x, type, vars, chains, mar, ...),
    density = bv_plot(x, type, vars, chains, mar, ...)
  )

  return(invisible(x))
}


#' @rdname plot.bvar
#'
#' @importFrom graphics par
bv_plot <- function(
  x,
  type = c("full", "trace", "density"),
  vars = NULL,
  chains = list(),
  mar = c(2, 2, 2, 0.5),
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(x) {if(!inherits(x, "bvar")) {
    stop("Please provide `bvar` objects to the chains.")
  }})

  type <- match.arg(type)


  # Prepare -----------------------------------------------------------------

  y <- x[["hyper"]]
  if(is.null(vars)) {
    vars <- c("ml", colnames(y))
  } else if(!all(vars %in% c("ml", colnames(y)))) {
    stop("Parameter named '", vars, "' not found.")
  }
  bounds <- vapply(vars[vars != "ml"], function(z) {
    c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])
  }, double(2))

  # Allow graphing draws from alternative chains
  chains_ml <- lapply(chains, function(x) {x[["ml"]]})
  chains_hy <- lapply(chains, function(x) {x[["hyper"]]})


  # Plot --------------------------------------------------------------------

  op <- par(mfrow = c(length(vars),
                      if(type == "full") {2} else {1}), mar = mar, ...)

  if("ml" %in% vars) { # Plot ml
    if(type != "density") { # i.e. full or trace
      plot_trace(x[["ml"]], name = "marginal likelihood", dots = chains_ml)
    }
    if(type != "trace") { # i.e. full or density
      plot_dens(x[["ml"]], name = "marginal likelihood", dots = chains_ml)
    }
  }
  for(name in colnames(bounds)) { # Plot hyperparameters
    if(type != "density") {
      plot_trace(y[, name], name, bounds[, name],
                 dots = lapply(chains_hy, function(x, name) {x[, name]}, name))
    }
    if(type != "trace") {
      plot_dens(y[, name], name, bounds[, name],
                dots = lapply(chains_hy, function(x, name) {x[, name]}, name))
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
