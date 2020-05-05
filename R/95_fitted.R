#' Fitted and residual methods for Bayesian VARs
#'
#' Calculates fitted or residual values for Bayesian VAR models generated with
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param x Object of class \code{bvar_fitted} / \code{bvar_resid}.
#' @inheritParams coef.bvar
#' @inheritParams plot.bvar
#'
#' @return Returns a numeric array of class \code{bvar_fitted} or
#' \code{bvar_resid} at the specified values.
#'
#' @seealso \code{\link{bvar}}
#'
#' @keywords BVAR analysis
#'
#' @export
#'
#' @importFrom stats fitted residuals
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
#'
#' # Get fitted values and adjust confidence bands to 10%, 50% and 90%
#' fitted(x, conf_bands = 0.10)
#'
#' # Get the residuals of variable 1
#' resid(x, vars = 1)
#' }
#' \dontrun{
#' # Get residuals and plot them
#' plot(residuals(x))
#' }
fitted.bvar <- function(
  object, type = c("quantile", "mean"), conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  type <- match.arg(type)

  X <- object[["meta"]][["X"]]
  N <- object[["meta"]][["N"]]
  M <- object[["meta"]][["M"]]
  betas <- coef(object, type, conf_bands)

  has_quants <- length(dim(betas)) == 3
  if(has_quants) {
    fit <- array(NA, c(dim(betas)[1], N, M),
      dimnames = list(dimnames(betas)[[1]], NULL, dimnames(betas)[[3]]))
    for(i in seq_len(dim(betas)[1])) {
      fit[i, , ] <- X %*% betas[i, , ]
    }
  } else {
    fit <- X %*% betas
  }
  class(fit) <- append("bvar_fitted", class(fit))

  return(fit)
}


#' @rdname fitted.bvar
#' @export
residuals.bvar <- function(
  object, type = c("quantile", "mean"), conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  type <- match.arg(type)

  fit <- fitted.bvar(object, type = type, conf_bands = conf_bands)

  Y <- object[["meta"]][["Y"]]

  has_quants <- length(dim(fit)) == 3
  if(has_quants) {
    resids <- array(NA, dim(fit), dimnames(fit))
    for(i in seq_len(dim(fit)[1])) {
      resids[i, , ] <- Y - fit[i, , ]
    }
  } else {
    resids <- Y - fit
  }
  class(resids) <- append("bvar_resid", class(resids))

  return(resids)
}


#' @rdname fitted.bvar
#' @export
plot.bvar_resid <- function(x, vars = NULL, mar = c(2, 2, 2, 0.5), ...) {

  if(!inherits(x, "bvar_resid")) {stop("Please provide a `bvar_resid` object.")}

  has_quants <- length(dim(x)) == 3
  if(has_quants) {x <- x["50%", , ]}
  M <- dim(x)[2]
  variables <- name_deps(variables = dimnames(x)[[2]], M = M)
  pos <- pos_vars(vars, variables, M)

  op <- par(mfrow = c(length(pos), 1), mar = mar, ...)
  for(i in pos) {
    plot(x[, i], main = paste("Residuals", variables[i]))
    abline(h = 0, lty = "dashed", col = "gray")
  }
  par(op)

  return(invisible(x))
}


#' @export
print.bvar_fitted <- function(x, digits = 2L, ...) {

  if(!inherits(x, "bvar_fitted")) {
    stop("Please provide a `bvar_fitted` object.")
  }

  print_fitted(x, digits, type = "fitted", ...)

  return(invisible(x))
}


#' @export
print.bvar_resid <- function(x, digits = 2L, ...) {

  if(!inherits(x, "bvar_resid")) {
    stop("Please provide a `bvar_resid` object.")
  }

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
    N <- dim(x)[2]; M <- dim(x)[3]; P <- dim(x)[1]
    variables <- name_deps(variables = dimnames(x)[[3]], M = M)
    top <- x["50%", 1:3, ]
    bot <- x["50%", (N - 2):N, ]
  } else {
    N <- dim(x)[1]; M <- dim(x)[2]
    variables <- name_deps(variables = dimnames(x)[[2]], M = M)
    top <- x[1:3, ]
    bot <- x[(N - 2):N, ]
  }

  cat("Numeric array (dimensions ", paste0(dim(x), collapse = ", "),  ")",
    " with ", type, " values from a BVAR.\n", sep = "")
  if(has_quants) {
    cat("Computed confidence bands: ",
      paste(dimnames(x)[[1]], collapse = ", "), "\n", sep = "")
  }
  cat("Average values:\n")
  for(var in seq_len(M)) {
    cat("\t", variables[var], ": ",
      paste0(round(top[, var], digits), collapse = ", "), ", [...], ",
      paste0(round(bot[, var], digits), collapse = ", "), "\n", sep = "")
  }

  return(invisible(x))
}
