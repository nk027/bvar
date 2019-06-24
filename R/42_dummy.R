#' Dummy prior settings
#'
#' @param mode Numeric scalar. Mode (or the like) of the parameter.
#' @param min Numeric scalar. Minimum allowed value.
#' @param max Numeric scalar. Maximum allowed value.
#' @param ... Other possible parameters such as sd or fun.
#'
#' @return Returns a list of class \code{bv_dummy.}
#'
#' @noRd
dummy <- function(
  mode = 1,
  min = 0.0001, max = 5,
  ...) {

  if(0 >= min || min >= max) {stop("Boundaries misspecified.")}
  if(mode < 0) {stop("Parameter misspecified.")}

  out <- list(mode = mode, min = min, max = max, ...)
  class(out) <- "bv_dummy"

  return(out)
}


#' Dummy prior settings
#'
#' Allows the creation of dummy observation priors for \code{\link{bv_priors}}.
#'
#' @param mode Numeric scalar. Mode (or the like) of the parameter.
#' @param sd Numeric scalar. Standard deviation (or the like) of the parameter.
#' @param min Numeric scalar. Minimum allowed value.
#' @param max Numeric scalar. Maximum allowed value.
#' @param fun Function taking \emph{Y}, \emph{lags} and the prior's parameter
#' \emph{par} to generate and return a named list with elements \emph{X} and
#' \emph{Y} (numeric matrices).
#'
#' @return Returns a named list of class \code{bv_dummy} for
#' \code{\link{bv_priors}}.
#' @export
#'
#' @examples
#' # Create a sum-of-coefficients prior
#' add_soc <- function(Y, lags, par) {
#'   soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
#'     diag(colMeans(Y[1:lags, ])) / par
#'   }
#'   Y_soc <- soc
#'   X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))
#'
#'   return(list("Y" = Y_soc, "X" = X_soc))
#' }
#'
#' soc <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)
#'
#' # Create a single-unit-root prior
#' add_sur <- function(Y, lags, par) {
#'   sur <- if(lags == 1) {Y[1, ] / par} else {
#'     colMeans(Y[1:lags, ]) / par
#'   }
#'   Y_sur <- sur
#'   X_sur <- c(1 / par, rep(sur, lags))
#'
#'   return(list("Y" = Y_sur, "X" = X_sur))
#' }
#'
#' sur <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_sur)
#'
#' # Adding them to the prior list with `bv_prior()`
#' priors_dum <- bv_priors(hyper = "auto", soc = soc, sur = sur)
bv_dummy <- function(
  mode = 1, sd = 1,
  min = 0.0001, max = 5,
  fun) {

  if(sd <= 0) {stop("Parameter sd misspecified.")}
  fun <- match.fun(fun)

  return(dummy(mode, min, max, sd = sd, fun = fun,
               coef = gamma_coef(mode, sd)))
}
