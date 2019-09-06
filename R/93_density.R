#' Density methods for Bayesian VARs
#'
#' Calculates densities of hyperparameters or coefficient values of Bayesian
#' VARs generated via \code{\link{bvar}}. Wraps standard
#' \code{\link[stats]{density}} functionality into a \code{list}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Optional character vector used to specify hyperparemeters to
#' retrieve the density of. The elements need to match the names of
#' hyperparameters (plus \code{"ml"}). Defaults to \code{NULL}, i.e. all
#' hyperparameters.
#' @param vars_response,vars_impulse Optional integer vector with the
#' positions of coefficient values to retrieve densities of.
#' \emph{vars_response} corresponds to a specific dependent variable,
#' \emph{vars_impulse} to an independent one. Note that the constant is found
#' at position one.
#' @param ... Fed to \code{\link[stats]{density}} or \code{\link[graphics]{par}}.
#'
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param mfrow Numeric vector. Rows for \code{\link[graphics]{par}}.
#' @param var,n_vars,lag Integer scalars.
#'
#' @return Returns a list with outputs of \code{\link[stats]{density}}.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @importFrom stats density
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get densities of standard hyperparameters
#' density(x)
#'
#' # Plot them
#' plot(density(x))
#'
#' # Only get the density of the marginal likelihood
#' density(x, vars = "ml")
#'
#' # Check out the constant's density on both dependents
#' plot(density(x, vars_impulse = 1))
#'
#' # Get the density of the 1st lag of variable 2's coefficients with
#' # respect to variable 1
#' idx <- independent_index(var = 2, n_vars = 2, lag = 1)
#' density(x, vars_response = 1, vars_impulse = idx)
#' }
density.bvar <- function(
  x, vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}


  # Get data and apply density --------------------------------------------

  prep <- prep_data(x, vars, vars_response, vars_impulse)
  data <- prep[["data"]]
  vars <- prep[["vars"]]

  out <- if(length(vars) == 1) {
    structure(list(density(data, ...)), names = vars)
  } else {structure(apply(data, 2, density, ...), names = vars)}

  class(out) <- "bvar_density"

  return(out)
}


#' @rdname density.bvar
#' @export
print.bvar_density <- function(x, ...) {

  if(!inherits(x, "bvar_density")) {
    stop("Please provide a `bvar_density` object.")
  }

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

  if(!inherits(x, "bvar_density")) {
    stop("Please provide a `bvar_density` object.")
  }

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
