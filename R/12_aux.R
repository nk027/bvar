#' Lag a matrix of time series
#'
#' Compute a lagged version of matrix to be used in vector autoregressions.
#' Multiple lags are added side by side.
#'
#' @param x Matrix (\eqn{N * M}) to lag.
#' @param lags Numeric scalar. Number of lags to create.
#'
#' @return Returns an \eqn{N * (M * lags)} matrix with consecutive lags at the
#' right.
#'
#' @noRd
lag_var <- function(x, lags) {

  x_rows <- nrow(x)
  x_cols <- ncol(x)

  x_lagged <- matrix(0, x_rows, lags * x_cols)
  for(i in 1:lags) {
    x_lagged[(lags + 1):x_rows, (x_cols * (i - 1) + 1):(x_cols * i)] <-
      x[(lags + 1 - i):(x_rows - i), (1:x_cols)]
  }

  return(x_lagged)
}


#' Compute gamma coefficients
#'
#' Compute the shape \emph{k} and scale \emph{theta} of a gamma
#' distribution via mode and standard deviation.
#'
#' @param mode Numeric scalar.
#' @param sd Numeric scalar.
#'
#' @return Returns a list with shape \emph{k} and scale paramter \emph{theta}.
#'
#' @noRd
gamma_coef <- function(mode, sd) {

  mode_sq <- mode ^ 2
  sd_sq <- sd ^ 2
  k <- (2 + mode_sq / sd_sq + sqrt((4 + mode_sq / sd_sq) * mode_sq / sd_sq)) / 2
  theta <- sqrt(sd_sq / k)

  return(list("k" = k, "theta" = theta))
}


#' Create parameter names
#'
#' Function to help name prior parameters. Accounts for multiple occurences
#' of \emph{psi} when \eqn{M > 1} by adding sequential numbers.
#'
#' @param x Character vector. Names of all relevant paramters.
#' @param M Integer scalar. Number of columns in the data.
#'
#' @return Returns a character vector of parameter names.
#'
#' @examples
#' bvar:::name_pars(c("lambda", "alpha"))
#' bvar:::name_pars(c("lambda", "psi"), M = 3)
#'
#' @noRd
name_pars <- function(x, M) {

  out <- Reduce(c, sapply(x, function(y) {if(y == "psi") {
    paste0(y, 1:M)
  } else {y}}))

  return(out)
}


#' Credible interval colour vector
#'
#' Create a character vector of colours for time series with credible
#' intervals, e.g. \code{\link{bv_plot_irf}} and \code{\link{bv_plot_fcast}}.
#' The central element is coloured \code{"black"}, the rest \code{"darkgray"}.
#'
#' @param P Integer scalar. Number of bands to plot.
#'
#' @return Returns a character vector of colours.
#'
#' @examples
#' bvar:::set_gray(3)
#'
#' @noRd
set_gray <- function(P) {

  n_gray <- if(P %% 2 == 0) {0} else {P %/% 2}

  return(c(rep("darkgray", n_gray), "black", rep("darkgray", n_gray)))
}


#' Get a subset of variables
#'
#' Helper functions to aid with variable selection in \code{\link{bv_plot_irf}}
#' and \code{\link{bv_plot_fcast}}.
#'
#' @param vars Vector of variables to subset to. Numeric or character.
#' @param variables Character vector of all variable names. Required if
#' \emph{vars} is provided as character vector.
#' @param M Integer scalar. Count of all variables.
#'
#' @return Returns a numeric vector with the positions of desired variables.
#'
#' @examples
#' # Assuming the variables are named.
#' bvar:::get_var_set("fx_rate", variables = c("gdp_pc", "fx_rate"))
#'
#' # Find via position
#' bvar:::get_var_set(c(1, 3), M = 3)
#'
#' # Get the full set
#' bvar:::get_var_set(NULL, M = 3)
#'
#' @noRd
get_var_set <- function(vars, variables, M) {

  if(is.null(vars)) {
    return(1:M)
  }
  if(is.numeric(vars)) {
    return(sort(vapply(vars, int_check,
                       min = 1, max = M, msg = "Variable(s) not found.",
                       integer(1))))
  }
  if(is.character(vars) && !is.null(variables)) {
    return(which(variables %in% vars))
  }

  stop("Variables not found.")
}
