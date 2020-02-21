#' Lag a matrix of time series
#'
#' Compute a lagged version of matrix to be used in vector autoregressions.
#' Multiple lags are added side by side.
#'
#' @param x Matrix (\eqn{N * M}) to lag.
#' @param lags Numeric scalar. Number of lags to create.
#'
#' @return Returns an \eqn{N * (M * lags)} matrix with consecutive lags on the
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
#' Compute the shape \emph{k} and scale \eqn{\theta} of a Gamma
#' distribution via mode and standard deviation.
#'
#' @param mode Numeric scalar.
#' @param sd Numeric scalar.
#'
#' @return Returns a list with shape \emph{k} and scale parameter \eqn{\theta}.
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
#' BVAR:::name_pars(c("lambda", "alpha"))
#' BVAR:::name_pars(c("lambda", "psi"), M = 3)
#'
#' @noRd
name_pars <- function(x, M) {

  out <- Reduce(c, sapply(x, function(y) {
    if(y == "psi") {paste0(y, 1:M)} else {y}}))

  return(out)
}


#' Credible interval symmetric filling
#'
#' Helper function to fill data, colours or similar things around credible
#' intervals. These are used in \code{\link{plot.bvar_irf}} and
#' \code{\link{plot.bvar_fcast}}.
#'
#' Note that HEX colours that need recycling are dealt some transparency.
#'
#' @param x Scalar or vector. The central element.
#' @param y Scalar or vector. Value(s) to surround the central element with.
#' The first value is closest and values may get recycled.
#' @param P Integer scalar. Number of total bands.
#'
#' @return Returns a vector or matrix (if x is a vector) of x, surrounded by y.
#'
#' @examples
#' BVAR:::fill_ci("malcolm", y = c("reese", "dewey"), P = 5)
#'
#' @noRd
fill_ci <- function(x, y, P) {

  n_y <- if(P %% 2 == 0) {
    message("No central position for x found.")
  } else {P %/% 2}

  fill <- rep(y, length.out = n_y)

  if(length(x) > 1) {
    n_row <- length(x)
    return(cbind(
      t(rev(fill))[rep(1, n_row), ], x, t(fill)[rep(1, n_row), ]))
  } else {
    return(c(rev(fill), x, fill))
  }
}


#' @noRd
fill_ci_na <- function(x, P) {

  # Corner case for data when quantiles are missing
  if(P == 2) {return(if(length(x > 1)) {cbind(x, NA)} else {c(x, NA)})}

  fill_ci(x = x, y = NA, P = P)
}


#' @noRd
fill_ci_col <- function(x, y, P) {

  if(length(y) == 1 && is_hex(y, alpha = FALSE)) {
    y <- paste0(y, transparance_hex(P))
  }

  fill_ci(x = x, y = y, P = P)
}


#' Get a transparency HEX code
#'
#' @param P Integer scalar. Number of total bands to determine number of codes.
#'
#' @return Returns a character vector of transparency codes. Note that there is
#' no central element for polygons and colours should be repeated symmetrically.
#'
#' @importFrom grDevices rgb
#'
#' @examples
#' # Retrieve auto-generated HEX codes
#' BVAR:::transparence_hex(21)
#'
#' @noRd
transparance_hex <- function(P) {

  n_trans <- P %/% 2
  out <- switch(n_trans, # Handpicked with love
    "FF", c("FF", "CC"), c("FF", "CC", "99"),
    c("FF", "CC", "99", "66"), c("FF", "CC", "99", "66", "33"))

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

  if(alpha) return(grepl("^#[0-9a-fA-F]{3,8}$", x))

  return(grepl("^#[0-9a-fA-F]{3,6}$", x))
}


#' Get variable positions
#'
#' Helper functions to aid with variable selection, e.g. in
#' \code{\link{plot.bvar_irf}} and \code{\link{plot.bvar_fcast}}.
#'
#' @param vars Vector of variables to subset to. Numeric or character.
#' @param variables Character vector of all variable names. Required if
#' \emph{vars} is provided as character vector.
#' @param M Integer scalar. Count of all variables.
#'
#' @return Returns a numeric vector with the positions of desired variables.
#'
#' @examples
#' # Use the names
#' BVAR:::pos_vars("fx_rate", variables = c("gdp_pc", "fx_rate"))
#'
#' # Subset via positions
#' BVAR:::pos_vars(c(1, 3), M = 3)
#'
#' # Get the full set
#' BVAR:::pos_vars(NULL, M = 3)
#'
#' @noRd
pos_vars <- function(vars, variables, M) {

  if(is.null(vars) || length(vars) == 0L) {
    return(1:M) # Full set
  }
  if(is.numeric(vars)) {
    return(sort(vapply(vars, int_check, # By position
      min = 1, max = M, msg = "Variable(s) not found.", integer(1))))
  }
  if(is.character(vars) && !is.null(variables)) {
    out <- do.call(c, lapply(vars, grep, variables)) # By name
    if(length(out) > 0) {return(out)}
  }

  stop("Variable(s) not found.")
}


#' Name dependent / explanatory variables
#'
#' Helper function to quickly generate names for variables.
#'
#' @param variables Character vector of all variable names.
#' @param M Integer scalar. Number of columns in the data.
#' @param lags Integer scalar. Number of lags applied in the model.
#'
#' @return Returns a character vector of names for the variables.
#'
#' @examples
#' # Get c("constant", "gdp-lag1", "cpi-lag1")
#' BVAR:::name_expl(c("gdp", "cpi"), lags = 1)
#'
#' # Get c("gdp", "cpi")
#' BVAR:::name_deps(c("gdp", "cpi"), M = 2)
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

  return(c("constant", paste0(rep(variables, lags), "-lag",
    rep(seq(lags), each = length(variables)))))
}


#' Compute log pdf of an inverse Gamma distribution
#'
#' Compute the logged pdf of a draw of a variable assumed to be inverse-Gamma
#' (IG) distributed with parameters \emph{scale} and \emph{shape}.
#'
#' @param x Numeric scalar. Draw of the IG-distributed variable.
#' @param scale Numeric scalar. Scale of the IG prior distribution.
#' @param shape Numeric scalar. Shape of the IG prior distribution.
#'
#' @return A numeric scalar of the draw's log-likelihood.
#'
#' @examples
#' # Computing log-likelihood of a draw with value 5
#' BVAR:::p_log_ig(5, 0.004, 0.004)
#'
#' @noRd
p_log_ig <- function(x, shape, scale) {

  return(scale * log(shape) - (scale + 1) * log(x) - shape / x - lgamma(scale))
}


#' Compute companion matrix
#'
#' Compute the companion form of the VAR coefficients.
#'
#' @param beta Numeric matrix. Non-companion form of the VAR coefficients.
#' @param K Integer scalar. Number of columns in the data.
#' @param M Integer scalar. Number of columns in the lagged data.
#' @param lags Integer scalar. Number of lags applied.
#'
#' @return Returns a numeric matrix with \emph{beta} in companion form.
#'
#' @noRd
get_beta_comp <- function(beta, K, M, lags) {

  beta_comp <- matrix(0, K - 1, K - 1)

  beta_comp[1:M, ] <- t(beta[2:K, ])
  if(lags > 1) { # Add block-diagonal matrix beneath VAR coefficients
    beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (lags - 1))
  }

  return(beta_comp)
}


#' Check whether a package is installed
#'
#' @param package Character scalar. Package to look for.
#'
#' @examples
#' # Check whether mad cow disease is present
#' has_package("BSE")
#'
#' @noRd
has_package <- function(package) {
  if(!requireNamespace(package, quietly = TRUE)) {
    stop("Package \'", package, "\' required for this method.", call. = FALSE)
  }

  return(NULL)
}


#' Construct matrix with paths for conditional forecasts
#'
#' @param path Numeric vector or matrix. Contains the path(s) of variable(s)
#' on which forecasts are conditioned on. Unrestricted future realisations
#' should be filled with \code{NA}. Note that not all variables can be
#' restricted at the same time.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param cond_var Optional vector. Containing variable names or positions in
#' case \emph{path} only restricts a subset of the variables.
#' @param variables Character vector of all variable names.
#' @param M Integer scalar. Count of all variables.
#'
#' @return Returns a matrix with the constrained paths of variables and
#' \code{NAs} for unrestricted values.
#'
#' @noRd
get_cond_mat <- function(path, horizon,
                         cond_var, variables, M) {

  cond_mat <- matrix(NA, horizon, M)
  if(is.vector(path)) {
    cond_var <- pos_vars(cond_var, variables, M)
    cond_mat[1:length(path), cond_var] <- path
  } else {
    if(ncol(path) > M) {
      stop("Path of conditions includes too many variables.")
    }
    if(ncol(path) == M){
      cond_mat[seq_len(nrow(path)), ] <- path
    } else {
      cond_var <- pos_vars(cond_var, variables, M)
      cond_mat[1:nrow(path), cond_var] <- path
    }
  }

  return(cond_mat)
}
