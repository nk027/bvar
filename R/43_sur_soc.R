#' Sum-of-coefficients and single-unit-root prior functions
#'
#' @param Y Numeric scalar. Mode (or the like) of the parameter.
#' @param lags Numeric scalar. Minimum allowed value.
#' @param par Numeric scalar. Maximum allowed value.
#'
#' @return Returns a list with \emph{Y} and \emph{X} extended with the
#' respective dummy observations.
#'
#' @noRd
.add_soc <- function(Y, lags, par) {
  soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
    diag(colMeans(Y[1:lags, ])) / par
  }
  Y_soc <- soc
  X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))

  return(list("Y" = Y_soc, "X" = X_soc))
}


#' @noRd
.add_sur <- function(Y, lags, par) {
  sur <- if(lags == 1) {Y[1, ] / par} else {
    colMeans(Y[1:lags, ]) / par
  }
  Y_sur <- sur
  X_sur <- c(1 / par, rep(sur, lags))

  return(list("Y" = Y_sur, "X" = X_sur))
}


#' Sum-of-coefficients dummy prior
#'
#' @export
#' @rdname bv_dummy
bv_soc <- function(mode = 1, sd = 1, min = 0.0001, max = 50) {

  bv_dummy(mode, sd, min, max, fun = .add_soc)
}

#' Single-unit-root dummy prior
#'
#' @export
#' @rdname bv_dummy
bv_sur <- function(mode = 1, sd = 1, min = 0.0001, max = 50) {

  bv_dummy(mode, sd, min, max, fun = .add_sur)
}