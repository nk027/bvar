#' Dummy prior settings
#'
#' Allows the creation of dummy observation priors for \code{\link{bv_priors}}.
#' See the Details section for information on common dummy priors.
#'
#' Dummy priors are often used to "reduce the importance of the deterministic
#' component implied by VARs estimated conditioning on the initial
#' observations" (Giannone et al., 2015, p. 440).
#' One such prior is the sum-of-coefficients (SOC) prior, which imposes the
#' notion that a no-change forecast is optimal at the beginning of a time
#' series. Its key parameter \eqn{\mu}{mu} controls the tightness - i.e. for
#' low values the model is pulled towards a form with as many unit roots as
#' variables and no cointegration.
#' Another such prior is the single-unit-root (SUR) prior, that allows for
#' cointegration relationships in the data. It pushes variables either towards
#' their unconditional mean or towards the presence of at least one unit root.
#' These priors are implemented via Theil mixed estimation, i.e. by adding
#' dummy-observations on top of the data matrix. They are available via the
#' functions \code{\link{bv_soc}} and \code{\link{bv_sur}}.
#'
#' @param fun Function taking \emph{Y}, \emph{lags} and the prior's parameter
#' \emph{par} to generate and return a named list with elements \emph{X} and
#' \emph{Y} (numeric matrices).
#' @inheritParams bv_mn
#'
#' @return Returns a named list of class \code{bv_dummy} for
#' \code{\link{bv_priors}}.
#'
#' @references
#'   Giannone, D. and Lenza, M. and Primiceri, G. E. (2015) Prior Selection for
#'   Vector Autoregressions. \emph{The Review of Economics and Statistics},
#'   \bold{97:2}, 436-451, \url{https://doi.org/10.1162/REST_a_00483}.
#'
#' @seealso \code{\link{bv_priors}}; \code{\link{bv_minnesota}}
#'
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
#' # Add the new custom dummy priors
#' bv_priors(hyper = "auto", soc = soc, sur = sur)
bv_dummy <- function(
  mode = 1, sd = 1,
  min = 0.0001, max = 5,
  fun) {

  sd <- num_check(sd, min = 0 + 1e-16, max = Inf,
    msg = "Parameter sd misspecified.")
  fun <- match.fun(fun)

  return(
    dummy(mode = mode, min = min, max = max, sd = sd, fun = fun,
      coef = gamma_coef(mode, sd))
  )
}


#' @rdname bv_dummy
#' @noRd
dummy <- function(
  mode = 1,
  min = 0.0001, max = 5,
  ...) {

  mode <- num_check(mode, min = 0, max = Inf,
    msg = "Invalid value for mode (outside of [0, Inf]).")
  min <- num_check(min, min = 0, max = max - 1e-16,
    msg = "Invalid value for min (outside of [0, max)).")
  max <- num_check(max, min = min + 1e-16, max = Inf,
    msg = "Invalid value for max (outside of (min, Inf]).")

  out <- structure(list(
    "mode" = mode, "min" = min, "max" = max, ...), class = "bv_dummy")

  return(out)
}
