
#' Sum-of-coefficients and single-unit-root prior creation functions
#'
#' @param Y Numeric matrix. Data to base the dummy observations on.
#' @param lags Integer scalar. Lag order of the model.
#' @param par Numeric scalar. Parameter value of the prior.
#'
#' @return Returns a list with \emph{Y} and \emph{X} extended with the
#' respective dummy observations.
#'
#' @noRd
.add_soc <- function(Y, lags, par) {
  soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
    diag(colMeans(Y[1:lags, ])) / par
  }
  X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))

  return(list("Y" = soc, "X" = X_soc))
}


#' @rdname .add_soc
#' @noRd
.add_sur <- function(Y, lags, par) {
  sur <- if(lags == 1) {Y[1, ] / par} else {
    colMeans(Y[1:lags, ]) / par
  }
  X_sur <- c(1 / par, rep(sur, lags))

  return(list("Y" = sur, "X" = X_sur))
}


#' @export
#' @describeIn bv_dummy Sum-of-coefficients dummy prior
bv_soc <- function(mode = 1, sd = 1, min = 0.0001, max = 50) {

  bv_dummy(mode = mode, sd = sd, min = min, max = max, fun = .add_soc)
}


#' @export
#' @describeIn bv_dummy Single-unit-root dummy prior
bv_sur <- function(mode = 1, sd = 1, min = 0.0001, max = 50) {

  bv_dummy(mode = mode, sd = sd, min = min, max = max, fun = .add_sur)
}
