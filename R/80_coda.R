#' Methods for \pkg{coda} Markov chain Monte Carlo objects
#'
#' Methods to convert parameter and/or coefficient draws from \code{\link{bvar}}
#' to \pkg{coda}'s \code{\link[coda]{mcmc}} (or \code{\link[coda]{mcmc.list}})
#' format for further processing.
#'
#' @name coda
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Character vector used to select variables. Elements are matched
#' to hyperparameters or coefficients. Coefficients may be matched based on
#' the dependent variable (by providing the name or position) or the
#' explanatory variables (by providing the name and the desired lag). See the
#' example section for a demonstration. Defaults to \code{NULL}, i.e. all
#' hyperparameters.
#' @param vars_response,vars_impulse Optional character or integer vectors used
#' to select coefficents. Dependent variables are specified with
#' \emph{vars_response}, explanatory ones with \emph{vars_impulse}. See the
#' example section for a demonstration.
#' @param chains List with additional \code{bvar} objects. If provided,
#' an object of class \code{\link[coda]{mcmc.list}} is returned.
#' @param ... Other parameters for \code{\link[coda]{as.mcmc}}.
#'
#' @return Returns a \pkg{coda} \code{\link[coda]{mcmc}} (or
#' \code{\link[coda]{mcmc.list}}) object.
#'
#' @seealso \code{\link{bvar}}; \code{\link[coda]{mcmc}};
#' \code{\link[coda]{mcmc.list}}
#'
#' @keywords BVAR coda MCMC analysis
#'
#' @examples
#' \donttest{
#' library("coda")
#'
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate two BVARs using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 750L, n_burn = 250L, verbose = FALSE)
#' y <- bvar(data, lags = 1, n_draw = 750L, n_burn = 250L, verbose = FALSE)
#'
#' # Convert the hyperparameter lambda
#' as.mcmc(x, vars = c("lambda"))
#'
#' # Convert coefficients for the first dependent, use chains in method
#' as.mcmc(structure(list(x, y), class = "bvar_chains"), vars = "CPIAUCSL")
#'
#' # Convert the coefs of variable three's first lag, use in the generic
#' as.mcmc(x, vars = "FEDFUNDS-lag1", chains = y)
#'
#' # Convert hyperparameters and constant coefficient values for variable 1
#' as.mcmc(x, vars = "lambda", "CPI", "constant")
#'
#' # Specify coefficent values to convert in alternative way
#' as.mcmc(x, vars_impulse = c("FED", "CPI"), vars_response = "UNRATE")
#' }
NULL


#' @rdname coda
as.mcmc.bvar <- function( # Dynamic export (zzz.R)
  x,
  vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  chains = list(), ...) {

  # Checks ---

  if(!inherits(x, "bvar")) {
    if(inherits(x[[1]], "bvar")) { # Allow chains to x
      chains <- x
      x <- x[[1]]
      chains[[1]] <- NULL
    } else {stop("Please provide a `bvar` object.")}
  }

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(z) {if(!inherits(z, "bvar")) {
    stop("Please provide `bvar` objects to the chains.")
  }})

  has_coda()


  # Get data and transform ---

  prep <- prep_data(x,
    vars = vars, vars_response = vars_response, vars_impulse = vars_impulse,
    chains = chains, check_chains = TRUE, Ms = TRUE, n_saves = TRUE)
  chains <- prep[["chains"]]

  if(!is.null(chains) && length(chains) > 0) {
    chains[["x"]] <- prep[["data"]]
    out <- coda::mcmc.list(... = lapply(chains, coda::as.mcmc, ...))
  } else {
    out <- coda::as.mcmc(prep[["data"]], ...)
  }

  return(out)
}


#' @rdname coda
as.mcmc.bvar_chains <- as.mcmc.bvar # Dynamic export (zzz.R)


#' @noRd
has_coda <- function() {has_package("coda")}

