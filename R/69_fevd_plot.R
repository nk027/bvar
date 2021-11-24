# #' Plotting method for Bayesian VAR forecast error variance decompositions
# #'
# #' Plotting method for forecast error variance decompositions obtained from
# #' \code{\link{fevd.bvar}}. Forecast error variance decompositions of all or a
# #' subset of the available variables can be plotted.
# #'
# #' @param x A \code{bvar_fevd} object, obtained from \code{\link{fevd.bvar}}.
# #' @param vars Optional numeric or character vector. Used to subset the plot's
# #' forecast error variance decompositions to certain variables by position
# #' or name (must be available). Defaults to \code{NULL}, i.e. all variables.
# #' @param variables Optional character vector. Names of all variables in the
# #' object. Used to subset and title. Taken from \code{x$variables} if available.
# #' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
# #' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
# #'
# #' @return Returns \emph{x} invisibly.
# #'
# #' @seealso \code{\link{bvar}}; \code{\link{fevd.bvar}}
# #'
# #' @keywords BVAR fevd irf analysis plot
# #'
# #' @export
# #'
# #' @examples
# #' \donttest{
# #' # Access a subset of the fred_qd dataset
# #' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
# #' # Transform it to be stationary
# #' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
# #'
# #' # Estimate a BVAR using one lag, default settings and very few draws
# #' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
# #'
# #' # Store FEVDs ex-post
# #' fevd(x) <- fevd(x)
# #'
# #' # Plot FEVDs for all available variables
# #' plot(fevd(x))
# #'
# #' # Subset to FEVDs in positions 2 and 3 via their name
# #' plot(fevd(x), vars = c(2, 3))
# #'
# #' # Subset via position and increase the plotted FEVD horizon
# #' plot(fevd(x, horizon = 20), vars = c("UNRATE", "FED"))
# #' }
# plot.bvar_fevd <- function(
#   x,
#   vars = NULL,
#   variables = NULL,
#   mar = c(2, 2, 2, 0.5),
#   ...) {

#   if(!inherits(x, "bvar_fevd")) {stop("Please provide a `bvar_fevd` object.")}
#   plot_fevd(x = x,
#             vars = vars, variables = variables,
#             mar = mar, ...)
# }


# #' @noRd
# plot_fevd <- function(
#   x,
#   vars = NULL,
#   variables = NULL,
#   mar = c(2, 2, 2, 0.5),
#   ...) {

#   # Checks ---

#   if(!inherits(x, "bvar") && !inherits(x, "bvar_fevd")) {
#     stop("Please provide a `bvar` or `bvar_fevd` object.")
#   }
#   if(inherits(x, "bvar")) {x <- fevd(x)}

#   # Prepare data ---

#   M <- dim(x[["fevd"]])[2]
#   fevd_mean <- apply(x[["fevd"]], c(2, 3, 4), mean)

#   # Prepare other arguments ---

#   variables <- name_deps(variables = if(is.null(variables)) {
#     x[["variables"]]} else {variables}, M = M)

#   pos <- pos_vars(vars, variables, M)

#   mfrow <- c(length(pos), 1)

#   .plot_fevd(x = fevd_mean, variables = variables,
#              pos = pos,
#              mar = mar, mfrow = mfrow, ...)

#   return(invisible(x))
# }


# #' Forecast error variance decomposition plot
# #'
# #' @param x Numeric array (3-dimensional) with data to plot. The first
# #' dimension contains responses, the second paths and the third impulses.
# #' @param variables Character vector with the names of variables.
# #' @param pos Integer vector. Positions of the forecast error variance
# #' decompositions to plot.
# #' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
# #' @param mfrow Numeric vector. Layout for \code{\link[graphics]{par}}.
# #' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
# #'
# #' @importFrom graphics par stats barplot
# #'
# #' @noRd
# .plot_fevd <- function(
#   x,
#   variables,
#   pos,
#   mar, mfrow,
#   ...) {

#   M <- dim(x)[1]
#   if(M > 5) M <- 5 # think of another way to make legend fitting
#   h <- dim(x)[2]

#   op <- par(mfrow = mfrow, mar = mar, ...)
#   for(i in pos) {
#     barplot(t(as.matrix(x[i, , ])),
#             main = paste("Forecast error variance decomposition of",
#                           variables[i]),
#             legend.text = variables,
#             args.legend = list(x = "bottom", inset = c(0, -0.25),
#                                bty = "n", ncol = M))
#   }
#   par(op)
# }
