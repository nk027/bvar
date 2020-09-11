
#' Density methods for Bayesian VARs
#'
#' Calculates densities of hyperparameters or coefficient draws from Bayesian
#' VAR models generated via \code{\link{bvar}}. Wraps standard
#' \code{\link[stats]{density}} outputs into a \code{list}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Fed to \code{\link[stats]{density}} or
#' \code{\link[graphics]{par}}.
#' @param mfrow Numeric vector. Rows for \code{\link[graphics]{par}}.
#' @param var,n_vars,lag Integer scalars. Retrieve the position of lag
#' \emph{lag} of variable \emph{var} given \emph{n_vars} total variables.
#' @inheritParams plot.bvar
#'
#' @return Returns a list with outputs of \code{\link[stats]{density}}.
#'
#' @seealso \code{\link{bvar}}; \code{\link[stats]{density}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom stats density
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
#' # Get densities of the hyperparameters
#' density(x)
#'
#' # Plot them
#' plot(density(x))
#'
#' # Only get the densities associated with dependent variable 1
#' density(x, vars_response = "CPI")
#'
#' # Check out the constant's densities
#' plot(density(x, vars_impulse = 1))
#'
#' # Get the densities of variable three's first lag
#' density(x, vars = "FEDFUNDS-lag1")
#'
#' # Get densities of lambda and the coefficients of dependent variable 2
#' density(x, vars = c("lambda", "UNRATE"))
#' }
density.bvar <- function(
  x, vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  ...) {

  # Get data and apply density ---

  prep <- prep_data(x, vars = vars,
    vars_response = vars_response, vars_impulse = vars_impulse)
  data <- prep[["data"]]
  vars <- prep[["vars"]]

  out <- if(length(vars) == 1) {
    structure(list(density(data, ...)), names = vars)
  } else {structure(apply(data, 2, density, ...), names = vars)}

  class(out) <- "bvar_density"

  return(out)
}


#' @export
print.bvar_density <- function(x, ...) {

  lapply(x, print, ...)

  return(invisible(x))
}


#' @rdname density.bvar
#' @export
#'
#' @importFrom graphics par
plot.bvar_density <- function(
  x,
  mar = c(2, 2, 2, 0.5),
  mfrow = c(length(x), 1),
  ...) {

  op <- par(mfrow = mfrow, mar = mar, ...)

  for(i in seq_along(x)) {
    plot(x[[i]], main = paste("Density of", names(x)[i]))
  }

  par(op)

  return(invisible(x))
}


#' @rdname density.bvar
#' @export
independent_index <- function(var, n_vars, lag) {

  x <- vapply(c(var, n_vars, lag), int_check, integer(1L))

  return(1 + x[2] * (x[3] - 1) + x[1])
}
