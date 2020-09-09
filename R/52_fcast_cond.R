
#' Conditional forecast computation
#'
#' Compute conditional forecasts using the algorithm of Waggoner and Zha (1999).
#'
#' @param constr_mat Numeric matrix with constrained paths of variables
#' and \code{NAs} for unrestricted values.
#' @param fcast_base Numeric matrix with unconditional forecasts without the
#' random shocks present in unconditional forecasts.
#' @param ortho_irf Numeric matrix with orthogonal impulse responses for all
#' variables. Computed by \code{\link{compute_irf}} or \code{\link{irf.bvar}}.
#' @param horizon Integer scalar. Horizon for which to compute forecasts.
#' @param M Integer scalar. Columns of \emph{Y}.
#'
#' @return Returns a numeric matrix with conditional forecasts.
#'
#' @references
#'   Waggoner, D. F., & Zha, T. (1999). Conditional Forecasts in Dynamic
#'   Multivariate Models. \emph{Review of Economics and Statistics},
#'   \bold{81:4}, 639-651, \url{https://doi.org/10.1162/003465399558508}.
#'
#' @importFrom stats rnorm
#'
#' @noRd
cond_fcast <- function(constr_mat, fcast_base, ortho_irf, horizon, M) {

  cond_fcast <- matrix(NA, horizon, M)
  # First get constrained shocks
  v <- sum(!is.na(constr_mat))
  s <- M * horizon
  r <- c(rep(0, v))
  R <- matrix(0, v, s)
  pos <- 1
  for(i in seq_len(horizon)) {
    for(j in seq_len(M)) {
      if(is.na(constr_mat[i, j])) {next}
      r[pos] <- constr_mat[i, j] - fcast_base[i, j]
      for(k in seq_len(i)) {
        R[pos, ((k - 1) * M + 1):(k * M)] <- ortho_irf[j, (i - k + 1) , ]
      }
      pos <- pos + 1
    }
  }

  R_svd <- svd(R, nu = nrow(R), nv = ncol(R))
  U <- R_svd[["u"]]
  P_inv <- diag(1/R_svd[["d"]])
  V1 <- R_svd[["v"]][, 1:v]
  V2 <- R_svd[["v"]][, (v + 1):s]
  eta <- V1 %*% P_inv %*% t(U) %*% r + V2 %*% rnorm(s - v)
  eta <- matrix(eta, M, horizon)

  # Use constrained shocks and unconditional forecasts (without shocks) to
  # create conditional forecasts
  for(h in seq_len(horizon)) {
    temp <- matrix(0, M, 1)
    for(k in seq_len(h)) {
      temp <- temp + ortho_irf[, (h - k + 1), ] %*% eta[ , k]
    }
    cond_fcast[h, ] <- fcast_base[h, ] + t(temp)
  }

  return(cond_fcast)
}


#' Build a constraint matrix for conditional forecasts
#'
#' @inheritParams bv_irf
#' @param variables Character vector of all variable names.
#' @param M Integer scalar. Count of all variables.
#'
#' @return Returns a numeric matrix with the constrained paths of variables and
#' \code{NAs} for unrestricted values.
#'
#' @noRd
get_constr_mat <- function(horizon, path, vars = NULL, variables = NULL, M) {

  pos <- pos_vars(vars, variables, M)
  constr_mat <- matrix(NA_real_, horizon, M)
  constr_mat[seq_len(nrow(path)), pos] <- path
  colnames(constr_mat) <- variables
  if(any(apply(constr_mat, 1, function(x) !any(is.na(x))))) {
    stop("One variable must be unrestricted at each point in time.")
  }

  return(constr_mat)
}
