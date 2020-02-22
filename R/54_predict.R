#' Predict method for Bayesian VARs
#'
#' Retrieves / calculates forecasts for Bayesian VARs generated via
#' \code{\link{bvar}}. If a forecast is already present and no settings are
#' supplied it is simply retrieved, otherwise it will be calculated ex-post.
#' To store a prediction you may want to assign the output of
#' \code{predict.bvar} to \code{object$fcast}. May also be used to update
#' confidence bands.
#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' Summary and print methods take in a \code{bvar_fcast} object.
#' @param ... A \code{bv_fcast} object or parameters to be fed into
#' \code{\link{bv_fcast}}. Contains settings for the forecast.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. \code{0.5} is always included.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{object}
#' is used for forecasting, others are dropped.
#' @param newdata Optional numeric matrix or dataframe. Used to base the
#' prediction on. Fitted values are used by default.
#' @param vars Optional numeric or character vector. Used to subset the summary
#' to certain variables by position or name (must be available). Defaults to
#' \code{NULL}, i.e. all variables.
#' @param value yes
#'
#' @return Returns a list of class \code{bvar_fcast} including forecasts
#' at desired confidence bands. See \code{\link{bvar}}.
#' The summary method returns a numeric array of forecast paths at the
#' specified confidence bands.
#'
#' @seealso \code{\link{plot.bvar_fcast}}; \code{\link{bv_fcast}}
#'
#' @keywords VAR BVAR forecasts prediction quantiles
#'
#' @export
#'
#' @importFrom stats predict
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Access forecast and update confidence bands
#' predict(x, conf_bands = 0.01)
#'
#' # Adjust, compute and store a longer forecast
#' x$fcast <- predict(x, horizon = 24L)
#'
#' # Lower draws, use `bv_fcast()` to set options and add confidence bands
#' predict(x, bv_fcast(24L), n_thin = 10L, conf_bands = c(0.05, 0.16))
#'
#' # Use new data to calculate a prediction
#' predict(x, newdata = matrix(rnorm(200), ncol = 2))
#'
#' # Get a summary of the last saved forecast
#' summary(x)
#'
#' # Limit the summary to variable #2
#' summary(x, vars = 2L)
#' }
predict.bvar <- function(
  object, ...,
  conf_bands, n_thin = 1L,
  newdata) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  dots <- list(...)
  fcast_store <- object[["fcast"]]


  # Calculate new forecast -----

  if(is.null(fcast_store) || length(dots) != 0L || !missing(newdata)) {

    # Setup ---

    fcast <- if(length(dots) > 0 && inherits(dots[[1]], "bv_fcast")) {
      dots[[1]]
    } else {bv_fcast(...)}

    # Checks
    n_pres <- object[["meta"]][["n_save"]]
    n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
      "Issue with n_thin. Maximum allowed is (n_draw - n_burn) / 10.")
    n_save <- int_check((n_pres / n_thin), min = 1)

    K <- object[["meta"]][["K"]]
    M <- object[["meta"]][["M"]]
    lags <- object[["meta"]][["lags"]]
    beta <- object[["beta"]]
    sigma <- object[["sigma"]]

    if(missing(newdata)) {
      Y <- object[["meta"]][["Y"]]
      N <- object[["meta"]][["N"]]
    } else {
      if(!all(vapply(newdata, is.numeric, logical(1))) || any(is.na(newdata)) ||
        ncol(newdata) != M) {stop("Problem with newdata.")}
      Y <- as.matrix(newdata)
      N <- nrow(Y)
    }

    # Conditional forecast
    conditional <- !is.null(fcast[["cond_path"]])
    if(conditional) {
      constr_mat <- get_constr_mat(horizon = fcast[["horizon"]],
        path = fcast[["cond_path"]],
        vars = fcast[["cond_vars"]], object[["variables"]], M)
      irf_store <- object[["irf"]]
      if(is.null(irf_store) || !irf_store[["setup"]][["identification"]] ||
        irf_store[["setup"]][["horizon"]] < fcast[["horizon"]]) {
        message("No suitable impulse responses found. Calculating...")
        irf_store <- irf.bvar(object,
          horizon = fcast[["horizon"]], identification = TRUE, fevd = FALSE,
          n_thin = n_thin)
      }
    }

    # Sampling ---

    fcast_store <- structure(list(
      "fcast" = array(NA, c(n_save, fcast[["horizon"]], M)),
      "setup" = fcast, "variables" = object[["variables"]], "data" = Y),
      class = "bvar_fcast")

    j <- 1
    for(i in seq_len(n_save)) {
      beta_comp <- get_beta_comp(beta[j, , ], K = K, M = M, lags = lags)
      fcast_base <- compute_fcast(
        Y = Y, K = K, M = M, N = N, lags = lags,
        horizon = fcast[["horizon"]],
        beta_comp = beta_comp, beta_const = beta[j, 1, ])

      if(conditional) { # Conditional uses impulse responses
        fcast_store[["fcast"]][i, , ] <- cond_fcast(
          constr_mat = constr_mat, fcast_base = fcast_base,
          ortho_irf = irf_store[["irf"]][j, , , ],
          horizon = fcast[["horizon"]], M = M)
      } else { # Unconditional gets noise
        fcast_store[["fcast"]][i, , ] <- fcast_base + t(crossprod(sigma[j, , ],
          matrix(rnorm(M * fcast[["horizon"]]), nrow = M)))
      }
      j <- j + n_thin
    }
  } # End new forecast

  if(is.null(fcast_store[["quants"]]) || !missing(conf_bands)) {
    fcast_store <- if(!missing(conf_bands)) {
      predict.bvar_fcast(fcast_store, conf_bands)
    } else {predict.bvar_fcast(fcast_store, c(0.16))}
  }

  return(fcast_store)
}


#' @noRd
`predict<-.bvar` <- function(object, value) {

  if(!inherits(object, "bvar")) {stop("Please use a `bvar` object.")}
  if(!inherits(value, "bvar_fcast")) {
    stop("Please provide a `bvar_fcast` object to assign.")
  }

  object[["fcast"]] <- value

  return(object)
}


#' @noRd
#' @export
#'
#' @importFrom stats predict quantile
predict.bvar_fcast <- function(object, conf_bands, ...) {

  if(!inherits(object, "bvar")) {
    stop("Please provide a `bvar_fcast` object.")
  }

  if(!missing(conf_bands)) {
    quantiles <- quantile_check(conf_bands)
    object[["quants"]] <- apply(object[["fcast"]], c(2, 3), quantile, quantiles)
  }

  return(object)
}


#' @rdname predict.bvar
#' @export
`predict<-` <- function(object, value) {UseMethod("predict<-", object)}
