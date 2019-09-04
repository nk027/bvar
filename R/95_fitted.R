#' Fitted and residual methods for Bayesian VARs
#'
#' Calculates fitted values / resiudals for Bayesian VARs generated via
#' \code{\link{bvar}}.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. 0.5 is always included.
#'
#' @param x Object of class \code{bvar_fitted} / \code{bvar_resid}.
#' @param digits Integer scalar. Fed to \code{\link[base]{round}} and applied to
#' numeric outputs (i.e. the quantiles).
#' @param vars Optional numeric vector. Used to subset the plot to certain
#' variables by position. Defaults to \code{NULL}, i.e. all variables.
#' @param mar Numeric vector. Margins for \code{\link[graphics]{par}}.
#' @param ... Other graphical parameters for \code{\link[graphics]{par}}.
#'
#' @return Returns a numeric array of class \code{bvar_fitted} /
#' \code{bvar_resid} with desired values at the specified confidence bands.
#'
#' @seealso \code{\link{bvar}}
#'
#' @export
#'
#' @importFrom stats fitted residuals
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Get fitted values and adjust confidence bands to 10%, 50% and 90%
#' fitted(x, conf_bands = 0.10)
#'
#' # Get residuals
#' residuals(x)
#' }
fitted.bvar <- function(object, conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  X <- object[["meta"]][["X"]]
  N <- object[["meta"]][["N"]]
  M <- object[["meta"]][["M"]]
  betas <- coef(object, conf_bands)

  has_quants <- length(dim(betas)) == 3
  if(has_quants) {
    fit <- array(NA, c(dim(betas)[1], N, M),
                 list(dimnames = dimnames(betas)[[1]], NULL, NULL))
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
residuals.bvar <- function(object, conf_bands = 0.5, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  fit <- fitted.bvar(object, conf_bands = conf_bands)

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
  pos <- get_var_set(vars, 1:M, M)

  op <- par(mfrow = c(length(pos), 1), mar = mar, ...)
  for(i in pos) {
    plot(x[, i], main = paste0("Variable #", i, "Residuals"))
    abline(h = 0, lty = "dashed", col = "gray")
  }
  par(op)

  return(invisible(x))
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

  cat("Numeric array (dimensions ", paste0(dim(x), collapse = ", "),  ")",
      " with ", type, " values from a BVAR.\n", sep = "")
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
