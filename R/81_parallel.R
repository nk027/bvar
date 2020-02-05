#' Parallel Bayesian Vector Autoregression
#'
#' Wrapper to speed up parallel computation using
#' \code{\link[parallel]{parLapply}}. Make sure to properly start and stop the
#' provided cluster.
#'
#' @param cl A \code{cluster} object obtained from
#' \code{\link[parallel]{makeCluster}}.
#' @param n_runs The number of parallel runs to calculate. Defaults to the
#' length of \emph{cl}, i.e. the number of registered nodes.
#' @inheritParams bvar
#'
#' @seealso \code{\link{bvar}}; \code{\link[parallel]{parLapply}}
#'
#' @keywords VAR BVAR mcmc parallel
#'
#' @export
#'
#' @examples
#' \donttest{
#' library("parallel")
#'
#' cl <- makeCluster(2L)
#' data <- matrix(rnorm(200), ncol = 2)
#'
#' # A singular run
#' x <- bvar(data, lags = 2)
#' # Two parallel runs
#' y <- par_bvar(cl, n_runs = 2, data = data, lags = 2)
#'
#' stopCluster(cl)
#'
#' # Plot lambda for all of the runs
#' plot(x, type = "full", vars = "lambda", chains = y)
#' }
par_bvar <- function(
  cl, n_runs = length(cl),
  data, lags,
  n_draw = 10000L, n_burn = 5000L, n_save, n_thin = 1L,
  priors = bv_priors(),
  mh = bv_mh(),
  fcast = NULL,
  irf = NULL) {

  # Checks ------------------------------------------------------------------

  if(!inherits(cl, "cluster")) {stop("Please provide a `cluster` object.")}

  # Maybe check whether it is actually loaded
  has_parallel()


  # Move checks to here -----------------------------------------------------

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
  if(missing(n_save)) {
    n_save <- int_check(((n_draw - n_burn) / n_thin), min = 1)
  } else {
    n_save <- int_check(n_save, min = 1L)
    n_draw <- int_check(n_save + n_burn, min = 1L)
  }

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


  # Get several BVARs -------------------------------------------------------

  out <- parallel::parLapply(cl, rep(list(data), n_runs),
    function(data, ...) { # This guy is spawned all alone, we need to load BVAR
      library("BVAR")
      bvar(data = data, ..., verbose = FALSE)
    }, lags = lags,
    n_draw = n_draw, n_burn = n_burn, n_save = n_save, n_thin = n_thin,
    priors = priors, mh = mh, fcast = fcast, irf = irf)

  class(out) <- "bvar_chains"

  return(out)
}


#' @noRd
has_parallel <- function() {
  if(!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package \'parallel\' required for this method.", call. = FALSE)
  }
}
