#' Density methods for Bayesian VARs
#'
#' Calculates densities of hyperparameters of Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Optional character vector used to subset the plot. The elements
#' need to match the names of hyperparameters (including \code{"ml"}). Defaults
#' to \code{NULL}, i.e. all variables.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Not used.
#'
#' @return Returns a list with outputs of \code{\link[stats]{density}}.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
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
#' }
density.bvar <- function(x, vars = NULL, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  data <- cbind("ml" = x[["ml"]], x[["hyper"]])

  if(missing(vars)) {
    vars <- colnames(data)
  } else if(!all(vars %in% colnames(data))) {
    stop("Parameter named '", vars, "' not found.")
  }

  out <- if(length(vars) == 1) {
    list(density(data[, vars], ...))
  } else {apply(data[, vars], 2, density, ...)}
  class(out) <- "bvar_density"

  return(out)
}


#' @rdname density.bvar
#' @export
#'
#' @importFrom stats print.denstiy
print.bvar_density <- function(x, ...) {

  lapply(x, stats:::print.density, ...)

  return(invisible(x))
}


#' @rdname density.bvar
#' @export
#'
#' @importFrom stats plot.density
#' @importFrom graphics par
plot.bvar_density <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  op <- par(mfrow = c(length(x), 1), mar = mar)

  for(i in seq_along(x)) {
    plot(x[[i]], main = paste("Density of", names(x)[i]), ...)
  }

  par(op)

  return(invisible(x))
}
