#' Minnesota prior settings
#'
#' Provide settings for the Minnesota prior to \code{\link{bv_priors}}. See the
#' Details section for further information.
#'
#' Essentially this prior imposes the hypothesis, that the individual variables
#' all follow random walk processes. This parsimonious specification typically
#' performs well in forecasts of macroeconomic time series and is often used
#' as a benchmark for evaluating accuracy (Kilian and Lütkepohl, 2017).
#' The key parameter is \eqn{\lambda} (\emph{lambda}), which controls the
#' tightness of the prior. The parameter \eqn{\alpha} (\emph{alpha}) governs
#' variance decay with increasing lag order, while \eqn{\psi} (\emph{psi}
#' controls the prior's standard deviation on lags of variables other than
#' the dependent.
#' The Minnesota prior is often refined with additional priors, trying to
#' minimise the importance of conditioning on initial observations. See
#' \code{\link{bv_dummy}} for more information on such priors.
#'
#' @param lambda List constructed via \code{\link{bv_lambda}}.
#' Arguments are \emph{mode}, \emph{sd}, \emph{min} and \emph{max}.
#' May also be provided as a numeric vector of length 4.
#' @param alpha List constructed via \code{\link{bv_alpha}}.
#' Arguments are \emph{mode}, \emph{min} and \emph{max}. High values for
#' \emph{mode} may affect invertibility of the augmented data matrix.
#' May also be provided as a mumeric vector of length 4.
#' @param psi List with elements \emph{scale}, \emph{shape} of the prior
#' as well as \emph{mode} and optionally \emph{min} and \emph{max}. The length
#' of these needs to match the number of variables (i.e. columns) in the data.
#' By default \emph{mode} is set automatically to the square-root of the
#' innovations variance after fitting an \eqn{AR(p)} model to the data. If
#' \code{\link[stats]{arima}} fails due to a non-stationary time series the
#' order of integration is incremented by 1. By default \emph{min} / \emph{max}
#' are set to \emph{mode} divided / multiplied by 100.
#' @param var Numeric scalar with the prior variance on the model's constant.
#' @param b Numeric matrix with the prior mean.
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
#'   Kilian, L. and Lütkepohl, H. (2017). \emph{Structural Vector Autoregressive
#'   Analysis}. Cambridge University Press,
#'   \url{https://doi.org/10.1017/9781108164818}
#'
#' @seealso \code{\link{bv_priors}}; \code{\link{bv_dummy}}
#'
#' @export
#'
#' @examples
#' # Adjust alpha and the Minnesota prior variance.
#' bv_mn(
#'   alpha = bv_alpha(mode = 0.5, sd = 1, min = 1e-12, max = 10),
#'   var = 1e6
#' )
#' # Optionally use a vector as shorthand
#' bv_mn(alpha = c(0.5, 1, 1e-12, 10), var = 1e6)
#'
#' # Only adjust lambda's standard deviation
#' bv_mn(lambda = bv_lambda(sd = 2))
bv_minnesota <- function(
  lambda = bv_lambda(),
  alpha = bv_alpha(),
  psi = bv_psi(), # scale, shape, mode
  var = 1e07,
  b = "auto") {

  # Input checks
  lambda <- lazy_priors(lambda)
  alpha <- lazy_priors(alpha)
  if(!inherits(psi, "bv_psi")) {stop("Please use `bv_psi()` to set psi.")}

  # Outputs
  out <- list("lambda" = lambda, "alpha" = alpha,
    "psi" = psi, "b" = b, "var" = var)

  class(out) <- "bv_minnesota"

  return(out)
}


#' @rdname bv_minnesota
#' @export
bv_mn <- bv_minnesota


#' @describeIn bv_minnesota Tightness of the Minnesota prior
#' @export
bv_lambda <- function(mode = 0.2, sd = 0.4, min = 0.0001, max = 5) {

  if(sd <= 0) {stop("Parameter sd misspecified.")}

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

  if(any(scale <= 0, shape <= 0)) {stop("Shape and/or scale misspecified.")}

  if(any(mode != "auto")) {
    mode <- vapply(mode, num_check, numeric(1L),
      min = 0, max = Inf, msg = "Issue with mode.")

    if(length(min) == 1 && min == "auto") {min <- mode / 100}
    if(length(max) == 1 && max == "auto") {max <- mode * 100}
    min <- vapply(min, num_check, numeric(1L),
      min = 0, max = max, msg = "Issue with min boundary.")
    max <- vapply(max, num_check, numeric(1L),
      min = min, max = Inf, "Issue with max boundary.")

  } else if(any(c(min != "auto", max != "auto"))) {
    stop("Boundaries are only adjustable with a given mode.")
  }

  if(length(mode) != length(min) || length(mode) != length(max)) {
    stop("The length of mode and boundaries diverge.")
  }

  out <- list("mode" = mode, "min" = min, "max" = max,
    "coef" = list("k" = shape, "theta" = scale)
  )

  class(out) <- "bv_psi"

  return(out)
}


#' @noRd
lazy_priors <- function(x) {

  if(!inherits(x, "bv_dummy")) {
    # Allow receiving length 4 numeric vectors
    if(length(x) == 4 && is.numeric(x)) {
      return(x = bv_lambda(x[1], x[2], x[3], x[4]))
    }
    stop("Please use `bv_lambda()` / `bv_alpha()` to set lambda / alpha.")
  }

  return(x)
}
