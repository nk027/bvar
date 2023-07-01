
#' Historical decomposition
#'
#' Function to compute a historical variance decomposition.
#'
#' @param type
#'
#' @return
#'
#' @keywords BVAR historical decomposition hd
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 600L, n_burn = 100L, verbose = FALSE)
#'
#' # Compute historical decomposition
#' hist_decomp(x, type = "mean"))
hist_decomp.bvar <- function(x, type = c("mean", "median")) {

  type <- match.arg(type)
  out <- compute_hd(x, type)

  return(out)
}


#' @rdname irf.bvar
#' @export
hist_decomp <- function(x, ...) {UseMethod("hist_decomp", x)}


#' @noRd
hist_decomp.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @noRd
compute_hd <- function(x, type,
  lags = x[["meta"]][["lags"]], N = x[["meta"]][["N"]],
  M = x[["meta"]][["M"]], K = x[["meta"]][["K"]]) {

  shock <- matrix(0, K - 1, M)
  shock[seq(M), ] <- t(chol(vcov(x, type = type)))
  comp <- companion(x, type = type)
  eps <- solve(shock, t(residuals(x, type = type)))

  kind_of_I <- cbind(diag(M), matrix(0, M, (lags - 1) * M))
  hd_comp <- array(0, dim = c(M, N + 1, M))
  tmp <- array(0, dim = c(K - 1, N + 1, M))

  for(j in seq_len(dim(hd_decomp)[3])) { # Over variables
    eps_tmp <- matrix(0, M, N + 1)
    eps_tmp[j, seq(2, N + 1)] <- eps[j, ]

    for(i in seq.int(2, dim(hd_decomp)[2])) { # Over observations
      tmp[, i, j] <- shock %*% eps_tmp[, i] + comp %*% tmp[, i - 1, j]
      hd_comp[, i, j] <- kind_of_I %*% tmp[, i, j]
    }
  }

  return(aperm(hd_comp, c(2, 1, 3)))
}
