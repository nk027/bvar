#' Minnesota prior settings
#'
#' Provide settings for the Minnesota prior to \code{\link{bv_priors}}.
#'
#' @param lambda List constructed via \code{\link{bv_lambda}}.
#' Possible parameters are \emph{mode}, \emph{sd}, \emph{min} and \emph{max}.
#' @param alpha List constructed via \code{\link{bv_alpha}}.
#' Possible parameters are \emph{mode}, \emph{min} and \emph{max}. High
#' values for \emph{mode} may affect invertibility of the augmented data matrix.
#' @param psi Named list with elements \emph{scale}, \emph{shape} and
#' \emph{mode}. Length needs to match the number of variables (i.e. columns) in
#' the data. By default parameters are set automatically as the squareroot of
#' the innovations variance after fitting an \eqn{AR(p)} model to the data.
#' @param var Numeric scalar with the prior variance on the model's constant.
#' @param b Numeric matrix with the prior mean.
#' @param mode Numeric scalar (/vector). Mode (or the like) of the parameter.
#' @param sd Numeric scalar with the standard deviation.
#' @param min Numeric scalar (/vector). Minimum allowed value.
#' @param max Numeric scalar (/vector). Maximum allowed value.
#' @param scale,shape  Numeric scalar. Scale and shape parameters of the Gamma
#' distribution.
#'
#' @return Returns a named list of class \code{bv_minnesota} with options for
#' \code{\link{bvar}}.
#' @export
#'
#' @examples
#' # Adjust alpha fully and the prior variance.
#' bv_mn(
#'   alpha = bv_alpha(mode = 0.5, sd = 1, min = 1e-12, max = 10),
#'   var = 1e6
#' )
#'
#' # Only adjust lambda's standard deviation
#' bv_mn(
#'   lambda = bv_lambda(sd = 2)
#' )
bv_mn <- function(
  lambda = bv_lambda(0.2, 0.4, 0.0001, 5), # mode, sd, min, max
  alpha = bv_alpha(2, 0.25, 1, 3), # mode, sd, min, max
  psi = bv_psi(0.004, 0.004, "auto"), # scale, shape, mode
  var = 1e07,
  b = "auto") {

  if(!inherits(lambda, "bv_dummy") && !inherits(alpha, "bv_dummy")) {
    stop("Please use `bv_lambda()` / `bv_alpha()` to set lambda / alpha.")
  }
  if(!inherits(psi, "bv_psi")) {
    stop("Please use `bv_psi()` to set psi.")
  }

  out <- list("lambda" = lambda, "alpha" = alpha,
              "psi" = psi, "b" = b, "var" = var)
  class(out) <- "bv_minnesota"

  return(out)
}


#' @export
#' @rdname bv_mn
bv_lambda <- function(mode = 0.2, sd = 0.4, min = 0.0001, max = 5) {

  if(sd <= 0) {stop("Parameter sd misspecified.")}

  return(dummy(mode, min, max, sd = sd, coef = gamma_coef(mode, sd)))
}


#' @export
#' @rdname bv_mn
bv_alpha <- function(mode = 2, sd = 0.25, min = 1, max = 3) {

  return(bv_lambda(mode = mode, sd = sd, min = min, max = max))
}


#' @export
#' @rdname bv_mn
bv_psi <- function(scale = 0.004, shape = 0.004, mode = "auto",
                   min = "auto", max = "auto") {

  if(any(scale <= 0, shape <= 0)) {stop("Parameters of psi misspecified.")}
  if(mode != "auto") {
    if(min == "auto") {min <- mode / 100}
    if(max == "auto") {max <- mode * 100}
    if(any(0 >= min, min >= max)) {stop("Boundaries misspecified.")}
  }

  out <- list(scale = scale, shape = shape, mode = mode, min = min, max = max)
  class(out) <- "bv_psi"

  return(out)
}
