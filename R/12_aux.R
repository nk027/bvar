#' Lag a matrix
#'
#' Compute a lagged version of a matrix to be used in vector autoregressions.
#' Higher lags are further to the right.
#'
#' @param x Matrix (\eqn{N * M}) to lag.
#' @param lags Integer scalar. Number of lags to apply.
#'
#' @return Returns an \eqn{N * (M * lags)} matrix with consecutive lags on the
#' right. The elements of the first \emph{lags} rows are 0.
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
#' Compute the shape \emph{k} and scale \emph{theta} of a Gamma
#' distribution via the mode and standard deviation.
#'
#' @param mode Numeric scalar.
#' @param sd Numeric scalar.
#'
#' @return Returns a list with shape \emph{k} and scale parameter \emph{theta}.
#'
#' @noRd
gamma_coef <- function(mode, sd) {

  mode_sq <- mode ^ 2
  sd_sq <- sd ^ 2
  k <- (2 + mode_sq / sd_sq + sqrt((4 + mode_sq / sd_sq) * mode_sq / sd_sq)) / 2
  theta <- sqrt(sd_sq / k)

  return(list("k" = k, "theta" = theta))
}


#' Name hyperparameters
#'
#' Function to help name hyperparameters. Accounts for multiple occurences
#' of \emph{psi} by adding sequential numbers.
#'
#' @param x Character vector. Parameter names.
#' @param M Integer scalar. Number of columns in the data.
#'
#' @return Returns a character vector of adjusted parameter names.
#'
#' @noRd
name_pars <- function(x, M) {

  out <- Reduce(c, sapply(x, function(y) {
    if(y == "psi") {paste0(y, 1:M)} else {y}}))

  return(out)
}


#' Fill credible intervals
#'
#' Helper function to fill data, colours or similar things based on credible
#' intervals. These are used in \code{\link{plot.bvar_irf}} and
#' \code{\link{plot.bvar_fcast}}.
#'
#' Note that transparency may get appended to recycled HEX colours. Also note
#' that no, i.e. a length 0 central element is required when drawing polygons.
#'
#' @param x Scalar or vector. The central element.
#' @param y Scalar or vector. Value(s) to surround the central element with.
#' The first value is closest, values may get recycled.
#' @param P Odd integer scalar. Number of total bands.
#'
#' @return Returns a vector or matrix (if \emph{x} is a vector) of \emph{x},
#' surrounded by \emph{y}.
#'
#' @noRd
fill_ci <- function(x, y, P) {

  n_y <- if(P %% 2 == 0) {
    stop("No central position for x found.")
  } else {P %/% 2}

  fill <- rep(y, length.out = n_y)

  if(length(x) > 1) { # Matrix
    n_row <- length(x)
    return(cbind(t(rev(fill))[rep(1, n_row), ], x, t(fill)[rep(1, n_row), ]))
  } else { # Vector
    return(c(rev(fill), x, fill))
  }
}


#' @noRd
fill_ci_na <- function(x, P) {

  # Corner case when quantiles are missing (t_back or conditional forecasts)
  if(P == 2) {return(if(length(x > 1)) {cbind(x, NA)} else {c(x, NA)})}

  fill_ci(x = x, y = NA, P = P)
}


#' @noRd
fill_ci_col <- function(x, y, P) {

  # Apply transparency to HEX colours
  if(length(y) == 1 && is_hex(y, alpha = FALSE)) {
    y <- paste0(y, alpha_hex(P))
  }

  fill_ci(x = x, y = y, P = P)
}


#' Get a transparency HEX code
#'
#' @param P Integer scalar. Number of total bands.
#'
#' @return Returns a character vector of transparency codes.
#'
#' @importFrom grDevices rgb
#'
#' @noRd
alpha_hex <- function(P) {

  n_trans <- P %/% 2
  out <- switch(n_trans, # Handpicked with love
    "FF", c("FF", "80"), c("FF", "A8", "54"),
    c("FF", "BF", "80", "40"), c("FF", "CC", "99", "66", "33"))

  if(is.null(out)) { # Let rgb() sort it out otherwise
    out <- substr(rgb(1, 1, 1, seq(1, 0, length.out = n_trans)), 8, 10)
  }

  return(out)
}


#' Check valid HEX colour
#'
#' @param x Character scalar or vector. String(s) to check.
#' @param alpha Logical scalar. Whether the string may contain alpha values.
#'
#' @return Returns a logical scalar or vector.
#'
#' @noRd
is_hex <- function(x, alpha = FALSE) {

  if(alpha) return(grepl("^#[0-9a-fA-F]{6,8}$", x))

  return(grepl("^#[0-9a-fA-F]{6,6}$", x))
}


#' Get variable positions
#'
#' Helper functions to aid with variable selection, e.g. in
#' \code{\link{plot.bvar_irf}} and \code{\link{plot.bvar_fcast}}.
#'
#' @param vars Numeric or character vector of variables to subset to.
#' @param variables Character vector of all variable names. Required if
#' \emph{vars} is provided as character vector.
#' @param M Integer scalar. Count of all variables.
#'
#' @return Returns a numeric vector with the positions of desired variables.
#'
#' @noRd
pos_vars <- function(vars, variables, M) {

  if(is.null(vars) || length(vars) == 0L) {
    return(1:M) # Full set
  }
  if(is.numeric(vars)) {
    return(vapply(vars, int_check, # By position
      min = 1, max = M, msg = "Variable(s) not found.", integer(1)))
  }
  if(is.character(vars) && !is.null(variables)) {
    out <- do.call(c, lapply(vars, grep, variables)) # By name
    if(length(out) > 0) {return(out)}
  }

  stop("Variable(s) not found.")
}


#' Name dependent / explanatory variables
#'
#' @param variables Character vector of all variable names.
#' @param M Integer scalar. Count of all variables.
#' @param lags Integer scalar. Number of lags applied.
#'
#' @return Returns a character vector of variable names.
#'
#' @noRd
name_deps <- function(variables, M) {

  if(is.null(variables)) {
    variables <- paste0("var", seq(M))
  } else if(length(variables) != M) {
    stop("Vector with variables is incomplete.")
  }

  return(variables)
}


#' @noRd
name_expl <- function(variables, M, lags) {

  if(is.null(variables)) {
    variables <- name_deps(variables, M)
  }
  explanatories <- c("constant", paste0(rep(variables, lags), "-lag",
    rep(seq(lags), each = length(variables))))

  return(explanatories)
}


#' Compute log distribution function of Inverse Gamma
#'
#' @param x Numeric scalar. Draw of the IG-distributed variable.
#' @param shape Numeric scalar.
#' @param scale Numeric scalar.
#'
#' @return Returns the log Inverse Gamma distribution function.
#'
#' @noRd
p_log_ig <- function(x, shape, scale) {

  return(shape * log(scale) - (shape + 1) * log(x) - scale / x - lgamma(shape))
}


#' Compute companion matrix
#'
#' Compute the companion form of the VAR coefficients.
#'
#' @param beta Numeric (\eqn{K * M}) matrix with VAR coefficients.
#' @param K Integer scalar. Number of columns in the independent data.
#' @param M Integer scalar. Number of columns in the dependent data.
#' @param lags Integer scalar. Number of lags applied.
#'
#' @return Returns a numeric (\eqn{K - 1 * K -1}) matrix with \emph{beta} in
#' companion form.
#'
#' @noRd
get_beta_comp <- function(beta, K, M, lags) {

  beta_comp <- matrix(0, K - 1, K - 1)

  beta_comp[1:M, ] <- t(beta[2:K, ]) # Kick constant
  if(lags > 1) { # Add block-diagonal matrix beneath VAR coefficients
    beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
  }

  return(beta_comp)
}


#' Check whether a package is installed
#'
#' @param package Character scalar.
#'
#' @noRd
has_package <- function(package) {

  if(!requireNamespace(package, quietly = TRUE)) {
    stop("Package \'", package, "\' required for this method.", call. = FALSE)
  }

  return(NULL)
}


#' Generate quantiles
#'
#' Check a vector of confidence bands and create quantiles from it.
#'
#' @param conf_bands Numeric vector of probabilities (\eqn{(0, 1)}).
#'
#' @return Returns a sorted, symmetric vector of quantiles.
#'
#' @noRd
quantile_check <- function(conf_bands) {

  conf_bands <- sapply(conf_bands, num_check,
    min = 0 + 1e-16, max = 1 - 1e-16, msg = "Confidence bands misspecified.")

  # Allow only returning the median
  if(length(conf_bands) == 1 && conf_bands == 0.5) {return(conf_bands)}

  # Sort and make sure we have no duplicates (thank mr float)
  quants <- sort(c(conf_bands, 0.5, (1 - conf_bands)))
  quants <- quants[!duplicated(round(quants, digits = 12L))]

  return(quants)
}
