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
#' Plot lambda for all of the runs
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


  # Get several BVARs -------------------------------------------------------

  out <- parLapply(cl, rep(list(data)),
    function(data, lags, # This guy is spawned all alone, we need to load BVAR
      n_draw, n_burn, n_save, n_thin, priors, mh, fcast, irf) {
      library("BVAR")
      bvar(data = data, lags, n_draw, n_burn, n_save, n_thin,
        priors, mh, fcast, irf, verbose = FALSE)
    }, lags, n_draw, n_burn, n_save, n_thin, priors, mh, fcast, irf)

  return(out)
}


#' @noRd
has_parallel <- function() {
  if(!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package \'parallel\' required for this method.", call. = FALSE)
  }
}
