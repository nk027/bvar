
#' Model fit in- and out-of-sample
#'
#' Functions to compute the root mean squared error and log predictive scores.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param holdout Optional numeric matrix or dataframe. Used for the
#' out-of-sample fit.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{x} is used
#' to calculate, others are dropped.
#' @param ... Not used.
#'
#' @return Returns a matrix with measures of model fit.
#'
#' @keywords BVAR RMSE LPS
#'
#' @export
#'
#' @importFrom stats dnorm resid
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data[seq(1, nrow(data) - 5), ], lags = 1,
#'   n_draw = 600L, n_burn = 100L, verbose = FALSE)
#'
#' # Compute RMSE
#' rmse(x)
#' lps(x, holdout = data[seq(nrow(data) - 4, nrow(data)), ])
#' }
rmse.bvar <- function(x, holdout, ...) {

  if(missing(holdout)) { # In-sample
    apply(resid(x, type = "mean"), 2, function(r) sqrt(sum(r^2) / length(r)))
  } else { # Out-of-sample
    fit <- apply(predict(x, horizon = NROW(holdout))$fcast, c(2, 3), mean)
    err <- fit - holdout
    apply(err, 2, function(r) sqrt(sum(r^2) / length(r)))
  }
}


#' @rdname rmse.bvar
#' @export
lps.bvar <- function(x, holdout, n_thin = 1L, ...) {

  n_pres <- x[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
    "Issue with n_thin. Maximum allowed is (n_draw - n_burn) / 10.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  if(missing(holdout)) { # In-sample
    # We need the mean and sd from these fits # To-do: adapt `resid`
    fit <- array(NA, dim = c(n_save, x[["meta"]][["N"]], x[["meta"]][["M"]]))
    i <- 1
    for(s in sample(n_pres, n_save)) {
      fit[i, , ] <- x[["meta"]][["X"]] %*% x[["beta"]][s, , ]
      i <- i + 1L
    }
    Y <- x[["meta"]][["Y"]]
  } else { # Out-of-sample
    fit <- predict(x, horizon = NROW(holdout))$fcast
    Y <- holdout
  }
  mu <- apply(fit, c(2, 3), mean)
  sd <- apply(fit, c(2, 3), sd)
  lps <- matrix(NA, NROW(mu), NCOL(mu))
  for(j in seq_len(ncol(lps))) {
    lps[, j] <- dnorm(Y[, j] - mu[, j], sd = sd[, j], log = TRUE)
  }

  return(lps)
}


#' @rdname rmse.bvar
#' @export
rmse <- function(x, ...) {UseMethod("rmse", x)}


#' @rdname rmse.bvar
#' @export
rmse.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @rdname rmse.bvar
#' @export
lps <- function(x, ...) {UseMethod("lps", x)}


#' @rdname rmse.bvar
#' @export
lps.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}
