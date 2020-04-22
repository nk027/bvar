#' Minnesota prior settings
#'
#' Provide settings for the Minnesota prior to \code{\link{bv_priors}}. See the
#' Details section for further information.
#'
#' Essentially this prior imposes the hypothesis, that individual variables
#' all follow random walk processes. This parsimonious specification typically
#' performs well in forecasts of macroeconomic time series and is often used as
#' a benchmark for evaluating accuracy (Kilian and Lütkepohl, 2017).
#' The key parameter is \eqn{\lambda}{lambda} (\emph{lambda}), which controls
#' the tightness of the prior. The parameter \eqn{\alpha}{alpha} (\emph{alpha})
#' governs variance decay with increasing lag order, while \eqn{\psi}{psi}
#' (\emph{psi}) controls the prior's standard deviation on lags of variables
#' other than the dependent.
#' The Minnesota prior is often refined with additional priors, trying to
#' minimise the importance of conditioning on initial observations. See
#' \code{\link{bv_dummy}} for more information on such priors.
#'
#' @param lambda List constructed via \code{\link{bv_lambda}}.
#' Arguments are \emph{mode}, \emph{sd}, \emph{min} and \emph{max}.
#' May also be provided as a numeric vector of length 4.
#' @param alpha List constructed via \code{\link{bv_alpha}}.
#' Arguments are \emph{mode}, \emph{sd}, \emph{min} and \emph{max}. High values
#' for \emph{mode} may affect invertibility of the augmented data matrix.
#' May also be provided as a numeric vector of length 4.
#' @param psi List with elements \emph{scale}, \emph{shape} of the prior
#' as well as \emph{mode} and optionally \emph{min} and \emph{max}. The length
#' of these needs to match the number of variables (i.e. columns) in the data.
#' By default \emph{mode} is set automatically to the square-root of the
#' innovations variance after fitting an \eqn{AR(p)}{AR(p)} model to the data.
#' If \code{\link[stats]{arima}} fails due to a non-stationary time series the
#' order of integration is incremented by 1. By default \emph{min} / \emph{max}
#' are set to \emph{mode} divided / multiplied by 100.
#' @param var Numeric scalar with the prior variance on the model's constant.
#' @param b Numeric scalar, vector or matrix with the prior mean. A scalar is
#' applied to all variables, with a default value of 1. Consider setting it to
#' 0 for growth rates. A vector needs to match the number of variables (i.e.
#' columns) in the data, with a prior mean per variable. If provided, a matrix
#' needs to have a column per variable (\eqn{M}), and \eqn{M * p + 1} rows,
#' where \eqn{p} is the number of lags applied.
#' @param mode,sd Numeric scalar. Mode / standard deviation of the
#' parameter. Note that the \emph{mode} of \emph{psi} is set automatically by
#' default, and would need to be provided as vector.
#' @param min,max Numeric scalar. Minimum / maximum allowed value. Note that
#' for \emph{psi} these are set automatically or need to provided as vectors.
#' @param scale,shape Numeric scalar. Scale and shape parameters of a Gamma
#' distribution.
#'
#' @return Returns a list of class \code{bv_minnesota} with options for
#' \code{\link{bvar}}.
#'
#' @references
#'   Kilian, L. and Lütkepohl, H. (2017). \emph{Structural Vector
#'   Autoregressive Analysis}. Cambridge University Press,
#'   \url{https://doi.org/10.1017/9781108164818}
#'
#' @seealso \code{\link{bv_priors}}; \code{\link{bv_dummy}}
#'
#' @export
#'
#' @examples
#' # Adjust alpha and the Minnesota prior variance.
#' bv_mn(alpha = bv_alpha(mode = 0.5, sd = 1, min = 1e-12, max = 10), var = 1e6)
#' # Optionally use a vector as shorthand
#' bv_mn(alpha = c(0.5, 1, 1e-12, 10), var = 1e6)
#'
#' # Only adjust lambda's standard deviation
#' bv_mn(lambda = bv_lambda(sd = 2))
#'
#' # Provide prior modes for psi (for a VAR with three variables)
#' bv_mn(psi = bv_psi(mode = c(0.7, 0.3, 0.9)))
bv_minnesota <- function(
  lambda = bv_lambda(),
  alpha = bv_alpha(),
  psi = bv_psi(), # scale, shape, mode
  var = 1e07,
  b = 1) {

  # Input checks
  lambda <- lazy_priors(lambda)
  alpha <- lazy_priors(alpha)
  if(!inherits(psi, "bv_psi")) {stop("Please use `bv_psi()` to set psi.")}
  var <- num_check(var, min = 1e-16, max = Inf,
    msg = "Issue with the prior variance var.")

  # Prior mean
  if(length(b) == 1 && b == "auto") {
    b <- 1
  } else if(is.numeric(b)) {
    if(!is.matrix(b)) { # Matrix dimensions are checked later
      b <- vapply(b, num_check, numeric(1L), min = -1e16, max = 1e16,
        msg = "Issue with prior mean b, please check the argument again.")
    }
  } else {stop("Issue with prior mean b, wrong type provided.")}

  # Outputs
  out <- structure(list(
    "lambda" = lambda, "alpha" = alpha, "psi" = psi, "b" = b, "var" = var),
    class = "bv_minnesota")

  return(out)
}


#' @rdname bv_minnesota
#' @export
bv_mn <- bv_minnesota


#' @describeIn bv_minnesota Tightness of the Minnesota prior
#' @export
bv_lambda <- function(mode = 0.2, sd = 0.4, min = 0.0001, max = 5) {

  sd <- num_check(sd, min = 0 + 1e-16, max = Inf,
    msg = "Parameter sd misspecified.")

  return(
    dummy(mode, min, max, sd = sd, coef = gamma_coef(mode = mode, sd = sd))
  )
}


#' @describeIn bv_minnesota Variance decay with increasing lag order
#' @export
bv_alpha <- function(mode = 2, sd = 0.25, min = 1, max = 3) {

  return(bv_lambda(mode = mode, sd = sd, min = min, max = max))
}


#' @describeIn bv_minnesota Prior standard deviation on other lags
#' @export
bv_psi <- function(
  scale = 0.004, shape = 0.004,
  mode = "auto", min = "auto", max = "auto") {

  # Checks ---

  scale <- num_check(scale, min = 1e-16, max = Inf,
    msg = "Invalid value for scale (outside of (0, Inf]")
  shape <- num_check(shape, min = 1e-16, max = Inf,
    msg = "Invalid value for shape (outside of (0, Inf]")

  # Check mode, min and max
  if(any(mode != "auto")) {
    mode <- vapply(mode, num_check, numeric(1L), min = 0, max = Inf,
      msg = "Invalid value(s) for mode (outside of [0, Inf]).")

    if(length(min) == 1 && min == "auto") {min <- mode / 100}
    if(length(max) == 1 && max == "auto") {max <- mode * 100}

    min <- vapply(min, num_check, numeric(1L), min = 0, max = Inf,
      msg = "Invalid value(s) for min (outside of [0, max)).")
    max <- vapply(max, num_check, numeric(1L), min = 0, max = Inf,
      msg = "Invalid value(s) for max (outside of (min, Inf]).")

    if(length(mode) != length(min) || length(mode) != length(max)) {
      stop("The length of mode and boundaries diverge.")
    }

    if(any(min >= max) || any(min > mode) || any(mode > max)) {
      stop("Invalid values for min / max.")
    }

  } else if(any(c(min != "auto", max != "auto"))) {
    stop("Boundaries are only adjustable with a given mode.")
  }

  # Outputs ---

  out <- structure(list(
    "mode" = mode, "min" = min, "max" = max,
    "coef" = list("k" = shape, "theta" = scale)
    ), class = "bv_psi")

  return(out)
}


#' @noRd
lazy_priors <- function(x) {

  if(!inherits(x, "bv_dummy")) {
    if(length(x) == 4 && is.numeric(x)) { # Allow length 4 numeric vectors
      return(x = bv_lambda(x[1], x[2], x[3], x[4]))
    }
    stop("Please use `bv_lambda()` / `bv_alpha()` to set lambda / alpha.")
  }

  return(x)
}
