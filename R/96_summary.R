#' Summary method for Bayesian VARs
#'
#' Retrieves several outputs of interest, including the median coefficient
#' matrix, the median variance-covariance matrix, and the Log-Likelihood. For
#' IRFs and forecasts separate methods for summarizing exist.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... Not used.
#'
#' @param x A \code{bvar_summary} object.
#'
#' @return Returns a list of class \code{bvar_summary} with elements that can
#' can be accessed individually:
#' \itemize{
#'   \item \code{bvar} - \emph{object}, the \code{bvar} object provided.
#'   \item \code{coef} - Coefficient values from \code{\link{coef.bvar}}.
#'   \item \code{vcov} - VCOV values from \code{\link{vcov.bvar}}.
#'   \item \code{logLik} - The Log-Likelihood from \code{\link{logLik.bvar}}.
#' }
#'
#' @seealso \code{\link{bvar}}; \code{\link{coef.bvar}};
#' \code{\link{logLik.bvar}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#' summary(x)
#' }
summary.bvar <- function(object, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  coef_x <- coef(object)
  vcov_x <- vcov(object)
  logLik_x <- logLik(object)

  out <- list(
    "bvar" = object,
    "coef" = coef_x,
    "vcov" = vcov_x,
    "logLik" = logLik_x
  )
  class(out) <- "bvar_summary"

  return(out)
}


#' @rdname summary.bvar
#' @export
print.bvar_summary <- function(x, ...) {

  print(x[["bvar"]])

  cat("\n"); print(x[["coef"]])
  cat("\n"); print(x[["vcov"]])
  cat("\n"); cat("Log-Likelihood:", x[["logLik"]], "\n")

  return(invisible(x))
}
