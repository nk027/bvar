#' Minnesota prior settings
#'
#' Provide settings for the Minnesota prior to \code{\link{bv_priors}}. See the
#' Details section for further information.
#'
#' Essentially this prior imposes the hypothesis, that the individual variables
#' all follow random walk processes. This parsimonious specification typically
#' performs well in forecasts of macroeconomic time series and is often used
#' as a benchmark for evaluating accuracy (Kilian and Lütkepohl, 2017).
#' The key parameter is \eqn{\lambda}, which controls the tightness of the
#' prior. The parameter \eqn{\alpha} governs variance decay with increasing lag
#' order, while \eqn{\psi} controls the prior's standard deviation on lags of
#' variables other than the dependent.
#' The Minnesota prior is often refined with additional priors, trying to
#' minimise the importance of conditioning on initial observations. See
#' \code{\link{bv_dummy}} for more information on such priors.
#'
#' @param lambda List constructed via \code{\link{bv_lambda}}.
#' Possible settings are \emph{mode}, \emph{sd}, \emph{min} and \emph{max}.
#' May also be provided as a mumeric vector of length 4.
#' @param alpha List constructed via \code{\link{bv_alpha}}.
#' Possible settings are \emph{mode}, \emph{min} and \emph{max}. High
#' values for \emph{mode} may affect invertibility of the augmented data matrix.
#' May also be provided as a mumeric vector of length 4.
#' @param psi List with elements \emph{scale}, \emph{shape} of the prior
#' as well as \emph{mode} and optionally \emph{min} and \emph{max}. The length
#' of these needs to match the number of variables (i.e. columns) in the data.
#' By default \emph{mode} is set automatically to the squareroot of the
#' innovations variance after fitting an \eqn{AR(p)} model to the data. By
#' default \emph{min} / \emph{max} are set to \emph{mode} divided / multiplied
#' by 100.
#' @param var Numeric scalar with the prior variance on the model's constant.
#' @param b Numeric matrix with the prior mean.
#' @param mode Numeric scalar (/vector). Mode (or the like) of the parameter.
#' @param sd Numeric scalar with the standard deviation.
#' @param min Numeric scalar (/vector). Minimum allowed value.
#' @param max Numeric scalar (/vector). Maximum allowed value.
#' @param scale,shape Numeric scalar. Scale and shape parameters of a Gamma
#' distribution.
#'
#' @return Returns a list of class \code{bv_minnesota} with options for
#' \code{\link{bvar}}.
#'
#' @references
#'     Kilian L, Lütkepohl H (2017). Structural Vector Autoregressive Analysis. Cambridge University Press.
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
#' # Optionally
#' bv_mn(alpha = c(0.5, 1, 1e-12, 10), var = 1e6)
#'
#' # Only adjust lambda's standard deviation
#' bv_mn(
#'   lambda = bv_lambda(sd = 2)
#' )
#'
#'
bv_mn <- function(
  lambda = bv_lambda(0.2, 0.4, 0.0001, 5), # mode, sd, min, max
  alpha = bv_alpha(2, 0.25, 1, 3), # mode, sd, min, max
  psi = bv_psi(0.004, 0.004, "auto"), # scale, shape, mode
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


#' @rdname bv_mn
#' @export
bv_minnesota <- bv_mn


#' @rdname bv_mn
#' @export
bv_lambda <- function(mode = 0.2, sd = 0.4, min = 0.0001, max = 5) {

  if(sd <= 0) {stop("Parameter sd misspecified.")}

  return(dummy(mode, min, max, sd = sd, coef = gamma_coef(mode, sd)))
}


#' @rdname bv_mn
#' @export
bv_alpha <- function(mode = 2, sd = 0.25, min = 1, max = 3) {

  return(bv_lambda(mode = mode, sd = sd, min = min, max = max))
}


#' @rdname bv_mn
#' @export
bv_psi <- function(scale = 0.004, shape = 0.004, mode = "auto",
                   min = "auto", max = "auto") {

  if(any(scale <= 0, shape <= 0)) {stop("Parameters of psi misspecified.")}
  if(any(mode != "auto")) {
    if(length(min) == 1 && min == "auto") {min <- mode / 100}
    if(length(max) == 1 && max == "auto") {max <- mode * 100}
    if(any(0 >= min, min >= max)) {stop("Boundaries misspecified.")}
  }
  if(length(mode) != length(min) || length(mode) != length(max)) {
    stop("Issue with mode and/or boundaries.")
  }

  out <- list("mode" = mode, "min" = min, "max" = max,
              "coef" = list("k" = shape, "theta" = scale))
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
