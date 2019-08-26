#' Predict method for Bayesian VARs
#'
#' Retrieves / calculates forecasts for Bayesian VARs generated via
#' \code{\link{bvar}}. If a forecast is already present and no settings are
#' supplied it is simply retrieved. If no forecast is present or settings are
#' provided one will be calculated ex-post. May also be used to update
#' confidence bands.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param ... A \code{bv_fcast} object or parameters to be fed into
#' \code{\link{bv_fcast}}. Contains settings for the forecast.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{x} is used
#' for forecasting, others are dropped. Defaults to the maximum number - i.e.
#' the number of saved draws in \emph{x}.
#' @param newdata Optional numeric matrix or dataframe. Used to base the
#' prediction on. Fitted values are used by default.
#'
#' @return
#' @export
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(200), ncol = 2)
#' x <- bvar(data, lags = 2)
#'
#' # Access forecast and update new confidence bands
#' predict(x, conf_bands = 0.01)
#'
#' # Compute and store a longer forecast
#' x$fcast <- predict(x, horizon = 24L)
#'
#' # Speed up by lowering draws and use bv_fcast() to set options
#' predict(x, bv_fcast(24L), n_thin = 10L)
#'
#' # Update the confidence bands
#' x$fcast <- predict(x$fcast, conf_bands = c(0.05, 0.16))
#'
#' # Use new data to calculate a prediction
#' predict(x, newdata = matrix(rnorm(200), ncol = 2))
#' }
predict.bvar <- function(x, ..., conf_bands, n_thin = 1L, newdata) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}


  # Retrieve / calculate fcast --------------------------------------------

  dots <- list(...)

  fcast_store <- x[["fcast"]]

  # If a forecast exists and no settings are provided
  if(is.null(fcast_store) || length(dots) != 0L || !missing(newdata)) {

    fcast <- if(length(dots) > 0 && inherits(dots[[1]], "bv_fcast")) {
      dots[[1]]
    } else {bv_fcast(...)}

    n_pres <- x[["meta"]][["n_save"]]
    n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
                        "Problematic value for parameter `n_thin`.")
    n_save <- int_check((n_pres / n_thin), min = 1)

    K <- x[["meta"]][["K"]]
    M <- x[["meta"]][["M"]]
    lags <- x[["meta"]][["lags"]]
    beta <- x[["beta"]]
    sigma <- x[["sigma"]]

    if(missing(newdata)) {
      Y <- x[["meta"]][["Y"]]
      N <- x[["meta"]][["N"]]
    } else {
      if(!all(vapply(newdata, is.numeric, logical(1))) || any(is.na(newdata)) ||
         ncol(newdata) != M) {stop("Problem with `newdata`.")}
      Y <- as.matrix(newdata)
      N <- nrow(Y)
    }

    fcast_store <- list(
      "fcast" = array(NA, c(n_save, fcast[["horizon"]], M)),
      "setup" = fcast
    )
    class(fcast_store) <- "bvar_fcast"

    j <- 1
    for(i in seq_len(n_save)) {
      beta_comp <- get_beta_comp(beta[j, , ], K, M, lags)
      fcast_store[["fcast"]][i, , ] <- compute_fcast(
        Y = Y, K = K, M = M, N = N, lags = lags,
        horizon = fcast[["horizon"]],
        beta_comp = beta_comp,
        beta_const = beta[j, 1, ], sigma = sigma[j, , ])
      j <- j + n_thin
    }
  }


  # Apply confidence bands ------------------------------------------------

  if(is.null(fcast_store[["quants"]]) || !missing(conf_bands)) {
    fcast_store <- if(!missing(conf_bands)) {
      predict.bvar_fcast(fcast_store, conf_bands)
    } else {predict.bvar_fcast(fcast_store, c(0.16))}
  }

  return(fcast_store)
}


#' @rdname predict.bvar
#' @export
predict.bvar_fcast <- function(x, conf_bands) {

  if(!inherits(x, "bvar_fcast")) {stop("Please provide a `bvar_fcast` object.")}

  if(!missing(conf_bands)) {
    quantiles <- quantile_check(conf_bands)
    x[["quants"]] <- apply(x[["fcast"]], c(2, 3), quantile, quantiles)
  }

  return(x)
}
