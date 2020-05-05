#' Parallel hierarchical Bayesian vector autoregression
#'
#' Wrapper for \code{\link{bvar}} to simplify parallel computation via
#' \code{\link[parallel]{parLapply}}. Make sure to properly start and stop the
#' provided cluster.
#'
#' @param cl A \code{cluster} object obtained from
#' \code{\link[parallel]{makeCluster}}.
#' @param n_runs The number of parallel runs to calculate. Defaults to the
#' length of \emph{cl}, i.e. the number of registered nodes.
#' @inheritParams bvar
#'
#' @return Returns a list of \code{bvar} objects.
#'
#' @seealso \code{\link{bvar}}; \code{\link[parallel]{parLapply}}
#'
#' @keywords BVAR Metropolis-Hastings MCMC priors hierarchical
#'
#' @export
#'
#' @examples
#' \donttest{
#' library("parallel")
#'
#' cl <- makeCluster(2L)
#'
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # A singular run using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 1000L, n_burn = 200L, verbose = FALSE)
#'
#' # Two parallel runs
#' y <- par_bvar(cl, n_runs = 2,
#'   data = data, lags = 1, n_draw = 1000L, n_burn = 200L)
#'
#' stopCluster(cl)
#' }
#' # Plot lambda for all of the runs
#' \dontrun{
#' plot(x, type = "full", vars = "lambda", chains = y)
#'
#' # Convert the hyperparameter lambda to a coda mcmc.list object
#' coda::as.mcmc(y, vars = "lambda")
#' }
par_bvar <- function(
  cl, n_runs = length(cl),
  data, lags,
  n_draw = 10000L, n_burn = 5000L, n_thin = 1L,
  priors = bv_priors(),
  mh = bv_mh(),
  fcast = NULL,
  irf = NULL) {

  # Checks ---

  if(!inherits(cl, "cluster")) {stop("Please provide a `cluster` object.")}

  # Maybe check whether it is actually loaded
  has_parallel()


  # Data
  if(!all(vapply(data, is.numeric, logical(1))) ||
     any(is.na(data)) || ncol(data) < 2) {
    stop("Problem with the data. Make sure it is numeric, without any NAs.")
  }

  Y <- as.matrix(data)

  # Integers
  lags <- int_check(lags, min = 1L, max = nrow(Y))
  n_draw <- int_check(n_draw, min = 1L)
  n_burn <- int_check(n_burn, min = 0L, max = n_draw - 1L,
    msg = "Issue with n_burn. Is n_burn < n_draw?")
  n_thin <- int_check(n_thin, min = 1L, max = ((n_draw - n_burn) / 10),
    msg = "Issue with n_thin. Maximum allowed is (n_draw - n_burn) / 10.")
  n_save <- int_check(((n_draw - n_burn) / n_thin), min = 1)

  # Constructors, required
  if(!inherits(priors, "bv_priors")) {
    stop("Please use `bv_priors()` to configure the priors.")
  }
  if(!inherits(mh, "bv_metropolis")) {
    stop("Please use `bv_mh()` to configure the Metropolis-Hastings step.")
  }
  # Not required
  if(!is.null(fcast) && !inherits(fcast, "bv_fcast")) {
    stop("Please use `bv_fcast()` to configure forecasts.")
  }
  if(!is.null(irf) && !inherits(irf, "bv_irf")) {
    stop("Please use `bv_irf()` to configure impulse responses.")
  }

  if(mh[["adjust_acc"]]) {n_adj <- as.integer(n_burn * mh[["adjust_burn"]])}


  # Get several BVARs ---

  out <- parallel::parLapply(cl, rep(list(data), n_runs),
    function(data, ...) { # This guy is spawned all alone, we need to load BVAR
      library("BVAR")
      bvar(data = data, ..., verbose = FALSE)
    }, lags = lags,
    n_draw = n_draw, n_burn = n_burn, n_thin = n_thin,
    priors = priors, mh = mh, fcast = fcast, irf = irf)

  class(out) <- "bvar_chains"

  return(out)
}


#' @noRd
has_parallel <- function() {has_package("parallel")}
