#' Conditional forecasts for Bayesian VARs
#'
#' Calculates conditional forecasts for Bayesian VARs generated via
#' \code{\link{bvar}} ex-post.

#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param path Numeric matrix. Specifies which paths the variables on which
#' to condition take.
#' @param cond_var Numeric or character vector. Contains names of variables
#' on which to condition or their position.
#' @param conf_bands Numeric vector of desired confidence bands to apply.
#' E.g. for bands at 5\%, 10\%, 90\% and 95\% set this to \code{c(0.05, 0.1)}.
#' Note that the median, i.e. \code{0.5} is always included.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{object}
#' is used for forecasting, others are dropped.
#' @param vars Optional numeric or character vector. Used to subset the summary
#' to certain variables by position or name (must be available). Defaults to
#' \code{NULL}, i.e. all variables.
#'
#' @return
#'
#' @seealso \code{\link{plot.bvar_fcast}}
#'
#' @keywords VAR BVAR conditional forecasts quantiles
#'
#' @export
#'
#' @importFrom stats predict
#'
#' @examples
scenario <- function(object, horizon, path, cond_var,
  conf_bands, n_thin = 1L, ...) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

  dots <- list(...)

  # Retrieve / calculate scen --------------------------------------------
  n_pres <- object[["meta"]][["n_save"]]
  n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
                      "Issue with n_thin.")
  n_save <- int_check((n_pres / n_thin), min = 1)

  horizon <- int_check(horizon, min = 1)

  K <- object[["meta"]][["K"]]
  M <- object[["meta"]][["M"]]
  lags <- object[["meta"]][["lags"]]
  beta <- object[["beta"]]
  sigma <- object[["sigma"]]

  Y <- object[["meta"]][["Y"]]
  N <- object[["meta"]][["N"]]


  irf_store <- object[["irf"]]
  if(is.null(irf_store)) {
    irf_store <- irf.bvar(object, ..., conf_bands, n_thin)
  }

  scen <- list(
    "horizon" = horizon,
    "path" = path,
    "pos" = pos
  )

  scen_store <- list(
    "scen" = array(NA, c(n_save, horizon, M)),
    "setup" = scen, "variables" = object[["variables"]],
    "data" = object[["meta"]][["Y"]]
  )
  class(scen_store) <- "bvar_scen"

  j <- 1
  for(i in seq_len(n_save)) {

    beta_comp <- get_beta_comp(beta[j, , ], K, M, lags)
    noshock_fcast <- compute_fcast(
      Y = Y, K = K, M = M, N = N, lags = lags,
      horizon = horizon,
      beta_comp = beta_comp,
      beta_const = beta[j, 1, ], sigma = sigma[j, , ],
      conditional = TRUE)

    ortho_irf <- irf_store[["irf"]][j, , , ]



    j <- j + n_thin
  }





  # Prepare outputs -------------------------------------------------------

  # Apply confidence bands
  if(is.null(fcast_store[["quants"]]) || !missing(conf_bands)) {
    fcast_store <- if(!missing(conf_bands)) {
      predict.bvar_fcast(fcast_store, conf_bands)
    } else {predict.bvar_fcast(fcast_store, c(0.16))}
  }

  return(fcast_store)
}

