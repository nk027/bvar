#' Density methods for Bayesian VARs
#'
#' Calculates densities of hyperparameters or coefficient values of BVARs
#' generated via \code{\link{bvar}}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Character vector used to specify hyperparemeters to
#' retrieve the density of. The elements need to match the names in
#' \code{x$hyper} (plus \code{"ml"}). Defaults to \code{NULL}, i.e. all
#' hyperparameters.
#' @param vars_response,vars_impulse Optional integer vectors with the
#' positions of coefficient values to retrieve densities of.
#' \emph{vars_response} corresponds to a specific dependent variable,
#' \emph{vars_impulse} to an independent one. Note that the constant is found
#' at position one.
#' @param ... Not used. Fed to \code{\link[stats]{density}}.
#'
#' @param var,n_vars,lag Integer scalars.
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
#'
#' # Check out the constant's density on both dependents
#' plot(density(x, vars_impulse = 1))
#'
#' # Get the density of the 1st lag of variable 2's coefficients with
#' respect to variable 1
#' idx <- independent_index(var = 2, n_vars = 2, lag = 1)
#' density(x, vars_response = 1, vars_impulse = idx)
#' }
density.bvar <- function(
  x, vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  # Create the matrix 'data' to apply quantiles over
  if(!is.null(vars_response) || !is.null(vars_impulse)) {
    vars_response <- get_var_set(vars_response, M = x[["meta"]][["M"]])
    vars_impulse <- get_var_set(vars_impulse, M = x[["meta"]][["K"]])

    n_col <- length(vars_response) * length(vars_impulse)
    beta <- x[["beta"]]

    data <- matrix(NA, nrow = x[["meta"]][["n_save"]], ncol = n_col)
    k <- 1
    for(i in seq_along(vars_response)) {for(j in seq_along(vars_impulse)) {
        data[, k] <- beta[, j, i]
        k <- k + 1
    }}
    vars <- paste0("dep", vars_response, "-ind", vars_impulse) # Names output
  } else { # We want to return hyperparameter densities
    data <- cbind("ml" = x[["ml"]], x[["hyper"]]) # Here we subset later
    if(is.null(vars)) {
      vars <- colnames(data)
    } else if(!all(vars %in% colnames(data))) {
      stop("Parameter named '", vars, "' not found.")
    }
    data <- data[, vars]
  }

  out <- if(length(vars) == 1) {
    structure(list(density(data, ...)), names = vars)
  } else {structure(apply(data, 2, density, ...), names = vars)}
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


#' @rdname density.bvar
#' @export
independent_index <- function(var, n_vars, lag) {
  x <- vapply(c(var, n_vars, lag), int_check, integer(1L))
  return(1 + x[2] * (x[3] - 1) + x[1])
}
