#' Method for converting hyperparameter chains to coda-compatible objects
#'
#' Method to convert chains of hyperparameters and marginal likelihood obtained
#' from \code{\link{bvar}} to objects compatible for further processing by
#' \code{\link{coda}} i.e., objects of class \code{\link[coda]{mcmc}} or
#' \code{\link[coda]{mcmc.list}}.
#' Resulting objects may be subset of all hyperparameters using \emph{vars}.
#' Multiple chains, i.e. comparable \code{bvar} objects may be converted using
#' the \emph{chains} argument.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Optional character vector used to subset the the converted
#' hyperparameters. The elements need to match the names of hyperparameters
#' (including \code{"ml"}). Defaults to \code{NULL}, i.e. all variables.
#' @param chains List with additional \code{bvar} objects. Contents are then
#' converted to an object of class \code{\link[coda]{mcmc.list}}.
#' @param ... Other parameters for \code{\link[coda]{as.mcmc}} and
#' \code{\link[coda]{as.mcmc.list}}.
#'
#' @seealso
#'
#' @keywords VAR BVAR coda mcmc object convergence
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#' y <- bvar(data, lags = 2)
#' z <- bvar(data, lags = 2)
#'
#' # Plot full traces and densities
#' as.mcmc(x, vars = c("ml", "lambda"))
#'
#' # Add second chain for further processing
#' as.mcmc(x, vars = c("ml", "lambda"), chains = list(y = y, z = z))
#' }
as.mcmc.bvar <- function(x, vars = NULL, chains = list(), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(z) {if(!inherits(z, "bvar")) {
    stop("Please provide `bvar` objects to the chains.")
  }})

  has_coda()

  y <- cbind(ml = x[["ml"]], x[["hyper"]])

  if(is.null(vars)) {
    vars <- c("ml", colnames(y))
  }

  if(length(chains) != 0){
    chains <- lapply(chains, function(z) {cbind(ml = z[["ml"]], z[["hyper"]])})
    apply(sapply(chains, colnames), 2, function(z) if(!all(vars %in% z)) {
      stop("Parameter(s) named '",
           paste0(vars[which(!vars %in% z)], collapse = ", "),
           "' not found in all provided chains.")
    })
    chains[[deparse(substitute(x))]] <- y
    chains <- lapply(chains, function(z) {z[ , which(colnames(z) %in% vars)]})
    out <- coda::as.mcmc.list(lapply(chains, coda::as.mcmc))
  } else {
    if(!all(vars %in% c("ml", colnames(y)))) {
      stop("Parameter(s) named '",
           paste0(vars[which(!vars %in% c("ml", colnames(y)))], collapse = ", "),
           "' not found.")
    }
    out <- coda::as.mcmc(y[ , which(colnames(y) %in% vars)], ...)
  }

  return(out)
}


#' @noRd
has_coda <- function() {
  if(!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \'coda\' required for this method.", call. = FALSE)
  }
}
