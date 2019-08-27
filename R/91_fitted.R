#' Fitted method for Bayesian VARs
#'
#' Retrieves / calculates forecasts for Bayesian VARs generated via
#' \code{\link{bvar}}. If a forecast is already present and no settings are
#' supplied it is simply retrieved. If no forecast is present or settings are
#' provided one will be calculated ex-post. May also be used to update
#' confidence bands.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{x} is used
#' for forecasting, others are dropped. Defaults to \code{100L}, to prevent
#' memory overflow.
#' @param ... Not used.
#'
#' @return
#' @export
#'
#' @examples
fitted.bvar <- function(x, conf_bands = 0.16, n_thin = 100L, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  n_pres <- x[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
                      "Problematic value for parameter `n_thin`.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  X <- x[["meta"]][["X"]]
  N <- x[["meta"]][["N"]]
  K <- x[["meta"]][["K"]]
  M <- x[["meta"]][["M"]]
  beta <- x[["beta"]]

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
print.bvar_fitted <- function(x, ...) {

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

  cat("Fitted values of a Bayesian VAR with ", N, " observations and ",
      M, " variables.\n", sep = "")
  if(has_quants) {
    cat("Computed confidence bands: ",
        paste(dimnames(x)[[1]], collapse = ", "), "\n", sep = "")
  }
  cat("Median fits:\n")
  for(var in seq_len(M)) {
    cat("\tVariable ", var, ": ",
        paste0(round(head[, var], 2L), collapse = ", "), ", [...], ",
        paste0(round(tail[, var], 2L), collapse = ", "), "\n", sep = "")
  }

  return(invisible(x))
}


residuals.bvar <- function(x, n_thin = 100L) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  fit <- fitted.bvar(x, conf_bands = 0.5, n_thin = n_thin)

  resids <- x[["meta"]][["Y"]] - fit
  class(resids) <- "bvar_resid"

  return(resids)
}
