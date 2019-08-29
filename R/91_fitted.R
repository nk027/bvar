#' Fitted and residual methods for Bayesian VARs
#'
#' Calculates fitted values / resiudals for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{object}
#' is used for forecasting, others are dropped. Defaults to \code{100L}, to
#' prevent memory overflow.
#'
#' @param x Object of class \code{bvar_fitted} / \code{bvar_resid}.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param ... Not used.
#'
#' @return Returns a numeric array of class \code{bvar_fitted} /
#' \code{bvar_resid} with desired values at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get fitted values and adjust confidence bands to 10%, 50% and 90%
#' fitted(x, conf_bands = 0.10)
#'
#' # Only get the median and increase the iterations
#' fitted(x, conf_bands = 0.5, n_thin = 10L)
#'
#' # Get residuals
#' residuals(x)
#' }
fitted.bvar <- function(object, conf_bands = 0.5, n_thin = 100L, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  n_pres <- object[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
                      "Problematic value for parameter `n_thin`.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  X <- object[["meta"]][["X"]]
  N <- object[["meta"]][["N"]]
  K <- object[["meta"]][["K"]]
  M <- object[["meta"]][["M"]]
  beta <- object[["beta"]]

  fit <- tryCatch(array(NA, c(n_save, N, M)), error = function(e) {
    stop(e, "Use `n_thin` to limit memory usage.")
  })

  j <- 1
  for(i in seq_len(n_save)) {
    fit[i, , ] <- X %*% beta[j, , ]
    j <- j + n_thin
  }

  quantiles <- quantile_check(conf_bands)
  fit <- apply(fit, c(2, 3), quantile, quantiles)
  class(fit) <- "bvar_fitted"

  return(fit)
}


#' @rdname fitted.bvar
#' @export
residuals.bvar <- function(object, conf_bands = 0.5, n_thin = 100L, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  fit <- fitted.bvar(object, conf_bands = conf_bands, n_thin = n_thin)

  has_quants <- length(dim(fit)) == 3
  if(has_quants) {
    resids <- array(NA, dim(fit), dimnames(fit))
    for(i in seq_len(dim(fit)[1])) {
      resids[i, , ] <- object[["meta"]][["Y"]] - fit[i, , ]
    }
  } else {
    resids <- object[["meta"]][["Y"]] - fit
  }
  class(resids) <- "bvar_resid"

  return(resids)
}


#' @rdname fitted.bvar
#' @export
print.bvar_fitted <- function(x, digits = 2L, ...) {

  if(!inherits(x, "bvar_fitted")) {stop("Please provide a `bvar_fitted` object.")}

  print_fitted(x, digits, type = "fitted", ...)

  return(invisible(x))
}


#' @rdname fitted.bvar
#' @export
print.bvar_resid <- function(x, digits = 2L, ...) {

  if(!inherits(x, "bvar_resid")) {stop("Please provide a `bvar_resid` object.")}

  print_fitted(x, digits, type = "residual", ...)

  return(invisible(x))
}


#' Fitted and residual print method
#'
#' @param x Numeric array with residual or fitted values of a \code{bvar}
#' object.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param type String indicating whether \emph{x} contains fitted or resiudal
#' values.
#'
#' @noRd
print_fitted <- function(
  x, digits = 2L,
  type = c("fitted", "residual"), ...) {

  type <- match.arg(type)

  has_quants <- length(dim(x)) == 3
  if(has_quants) {
    N <- dim(x)[2]
    M <- dim(x)[3]
    P <- dim(x)[1]
    head <- x["50%", 1:3, ]
    tail <- x["50%", (N - 2):N, ]
  } else {
    N <- dim(x)[1]
    M <- dim(x)[2]
    head <- x[1:3, ]
    tail <- x[(N - 2):N, ]
  }

  cat(gsub("^(.)(.*)", "\\U\\1\\L\\2", type, perl = TRUE),
      " values of a Bayesian VAR with ", N, " observations and ",
      M, " variables.\n", sep = "")
  if(has_quants) {
    cat("Computed confidence bands: ",
        paste(dimnames(x)[[1]], collapse = ", "), "\n", sep = "")
  }
  cat("Median values:\n")
  for(var in seq_len(M)) {
    cat("\tVariable ", var, ": ",
        paste0(round(head[, var], digits), collapse = ", "), ", [...], ",
        paste0(round(tail[, var], digits), collapse = ", "), "\n", sep = "")
  }

  return(invisible(x))
}
