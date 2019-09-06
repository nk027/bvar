#' Method for coda Markov chain Monte Carlo objects
#'
#' Method to convert chains of hyperparameters and marginal likelihoods obtained
#' from \code{\link{bvar}} or coefficent values to objects compatible for
#' further processing with \pkg{coda}, i.e., objects of class
#' \code{\link[coda]{mcmc}} or \code{\link[coda]{mcmc.list}}.
#' Multiple chains, that is comparable \code{bvar} objects may be converted
#' using the \emph{chains} argument.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Optional character vector used to subset the converted
#' hyperparameters. The elements need to match the names of hyperparameters
#' (plus \code{"ml"}). Defaults to \code{NULL}, i.e. all variables.
#' @param vars_response,vars_impulse Optional integer vector with the
#' positions of coefficient values to convert. \emph{vars_response} corresponds
#' to a specific dependent variable, \emph{vars_impulse} to an independent one.
#' Note that the constant is found at position one.
#' @param chains List with additional \code{bvar} objects. If provided, contents
#' are converted to an object of class \code{\link[coda]{mcmc.list}}.
#' @param ... Other parameters for \code{\link[coda]{as.mcmc}} and
#' \code{\link[coda]{as.mcmc.list}}.
#'
#' @seealso \code{\link{bvar}}; \code{\link[coda]{mcmc}}
#'
#' @keywords VAR BVAR coda mcmc convergence
#'
#' @export
#'
#' @examples
#' \donttest{
#' library("coda")
#'
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#' y <- bvar(data, lags = 2)
#'
#' # Convert hyperparameter lambda and the marginal likelihood
#' as.mcmc(x, vars = c("ml", "lambda"))
#'
#' # Add second chain for further processing
#' as.mcmc(x, vars = c("ml", "lambda"), chains = list(y = y))
#' }
as.mcmc.bvar <- function(
  x,
  vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  chains = list(), ...) {

  # Checks ------------------------------------------------------------------

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(z) {if(!inherits(z, "bvar")) {
    stop("Please provide `bvar` objects to the chains.")
  }})

  has_coda()


  # Get data and transform --------------------------------------------------

  prep <- prep_data(x, vars, vars_response, vars_impulse,
                    chains, check_chains = TRUE, n_saves = TRUE)
  data <- prep[["data"]]
  vars <- prep[["vars"]]
  chains <- prep[["chains"]]

  if(!is.null(chains) && length(chains) > 0) {
    chains[["x"]] <- data
    out <- coda::mcmc.list(... = lapply(chains, coda::as.mcmc, ...))
  } else {
    out <- coda::as.mcmc(data, ...)
  }

  return(out)
}


#' @noRd
has_coda <- function() {
  if(!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \'coda\' required for this method.", call. = FALSE)
  }
}
