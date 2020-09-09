
#' Prior settings
#'
#' Function to provide priors and their parameters to \code{\link{bvar}}. Used
#' for adjusting the parameters treated as hyperparameters, the Minnesota prior
#' and adding various dummy priors through the ellipsis parameter.
#' Note that treating \eqn{\psi}{psi} (\emph{psi}) as a hyperparameter in a
#' model with many variables may lead to very low acceptance rates and thus
#' hinder convergence.
#'
#' @param hyper Character vector. Used to specify the parameters to be treated
#' as hyperparameters. May also be set to \code{"auto"} or \code{"full"} for
#' an automatic / full subset. Other allowed values are the Minnesota prior's
#' parameters \code{"lambda"}, \code{"alpha"} and \code{"psi"} as well as the
#' names of additional dummy priors included via \emph{...}.
#' @param mn List of class \code{"bv_minnesota"}. Options for the Minnesota
#' prior, set via \code{\link{bv_mn}}.
#' @param ... Optional lists of class \code{bv_dummy} with options for
#' dummy priors. \bold{Must be assigned a name in the function call}. Created
#' with \code{\link{bv_dummy}}.
#'
#' @return Returns a named list of class \code{bv_priors} with options for
#' \code{\link{bvar}}.
#'
#' @keywords priors hierarchical Minnesota dummy settings
#'
#' @seealso \code{\link{bv_mn}}; \code{\link{bv_dummy}}
#'
#' @export
#'
#' @examples
#' # Extend the hyperparameters to the full Minnesota prior
#' bv_priors(hyper = c("lambda", "alpha", "psi"))
#' # Alternatively
#' # bv_priors("full")
#'
#' # Add a dummy prior via `bv_dummy()`
#'
#' # Re-create the single-unit-root prior
#' add_sur <- function(Y, lags, par) {
#'   sur <- if(lags == 1) {Y[1, ] / par} else {
#'     colMeans(Y[1:lags, ]) / par
#'   }
#'   Y_sur <- sur
#'   X_sur <- c(1 / par, rep(sur, lags))
#'
#'   return(list("Y" = Y_sur, "X" = X_sur))
#' }
#' sur <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_sur)
#'
#' # Add the new prior
#' bv_priors(hyper = "auto", sur = sur)
bv_priors <- function(
  hyper = "auto",
  mn = bv_mn(),
  ...) {

  # Check inputs ---

  if(!inherits(mn, "bv_minnesota")) { # Require Minnesota prior
    stop("Please use `bv_mn()` to set the Minnesota prior.")
  }
  dots <- list(...)
  if(!all(vapply(dots, inherits, TRUE, "bv_dummy"))) {
    stop("Please use `bv_dummy()` to set dummy priors.")
  }
  if(hyper[[1]] == "auto") {
    hyper <- c(if(!is.null(mn)) {"lambda"}, names(dots))
  } else {
    full <- c(if(!is.null(mn)) {c("lambda", "alpha", "psi")}, names(dots))
    if(hyper[[1]] == "full") {
      hyper <- full
    } else {
      if(!all(hyper %in% full)) {stop("Hyperprior not found.")}
    }
  }

  # Prepare output ---

  out <- if(!is.null(mn)) {
    structure(list(hyper = hyper,
      lambda = mn[["lambda"]], alpha = mn[["alpha"]],
      psi = mn[["psi"]], var = mn[["var"]], b = mn[["b"]],
      ..., dummy = names(list(...))
      ), class = "bv_priors")
  } else {
    structure(list(hyper = hyper,
      ..., dummy = names(list(...))), class = "bv_priors")
  }

  return(out)
}
