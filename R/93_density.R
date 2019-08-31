#' Density methods for Bayesian VARs
#'
#' Calculates densities of hyperparameters or coefficient values of BVARs
#' generated via \code{\link{bvar}}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param hyper Character vector used to specify hyperparemeters to
#' retrieve the density of. The elements need to match the names in
#' \code{x$hyper} (plus \code{"ml"}). Defaults to \code{NULL}, i.e. all
#' hyperparameters.
#' @param var_dep,var_ind Optional integer vectors with the positions of
#' coefficient values to retrieve densities of. \emph{var_dep} corresponds to
#' a specific dependent variable, \emph{var_ind} to an independent one. Note
#' that the constant is found at position one.
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
#' density(x, hyper = "ml")
#' }
density.bvar <- function(
  x, hyper = NULL,
  var_dep = NULL, var_ind = NULL,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  # Create the matrix data to apply quantiles over
  if(!is.null(var_dep) || !is.null(var_ind)) {
    var_dep <- get_var_set(var_dep, M = x[["meta"]][["M"]])
    var_ind <- get_var_set(var_ind, M = x[["meta"]][["K"]])

    n_col <- length(var_dep) * length(var_ind)
    beta <- x[["beta"]]

    data <- matrix(NA, nrow = x[["meta"]][["n_save"]], ncol = n_col)
    k <- 1
    for(i in seq_along(var_dep)) {for(j in seq_along(var_ind)) {
        data[, k] <- beta[, j, i]
        k <- k + 1
    }}
    hyper <- paste0("dep", var_dep, "-ind", var_ind) # Used to name output
  } else { # If we want to return hyperparameters
    data <- cbind("ml" = x[["ml"]], x[["hyper"]]) # Subset later
    if(is.null(hyper)) {
      hyper <- colnames(data)
    } else if(!all(hyper %in% colnames(data))) {
      stop("Parameter named '", hyper, "' not found.")
    }
    data <- data[, hyper]
  }

  out <- if(length(hyper) == 1) {
    structure(list(density(data, ...)), names = hyper)
  } else {structure(apply(data, 2, density, ...), names = hyper)}
  class(out) <- "bvar_density"

  return(out)
}


#' @rdname density.bvar
#' @export
print.bvar_density <- function(x, ...) {

  lapply(x, print, ...)

  return(invisible(x))
}


#' @rdname density.bvar
#' @export
#'
#' @importFrom graphics par
plot.bvar_density <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  op <- par(mfrow = c(length(x), 1), mar = mar)

  for(i in seq_along(x)) {
    plot(x[[i]], main = paste("Density of", names(x)[i]), ...)
  }

  par(op)

  return(invisible(x))
}
