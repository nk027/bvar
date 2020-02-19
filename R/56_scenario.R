#' Conditional forecasts for Bayesian VARs
#'
#' Calculates conditional forecasts for Bayesian VARs generated via
#' \code{\link{bvar}} ex-post.

#'
#' @param object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param horizon Integer scalar. Specifies the horizon for which forecasts
#' should be computed.
#' @param path Numeric matrix or vector. Specifies which paths the variables
#' on which to condition take. If a matrix, then size must be horizon x M.
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
scenario <- function(object, horizon, path, cond_var = NULL,
  conf_bands, n_thin = 1L) {

  if(!inherits(object, "bvar")) {stop("Please provide a `bvar` object.")}

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

  variables <- object[["variables"]]

  cond_mat <- get_cond_mat(path, horizon, cond_var, variables, M)

  irf_store <- object[["irf"]][["irf"]]
  if(is.null(irf_store)) {
    irf_store <- irf.bvar(object, n_thin = n_thin)[["irf"]]
  }

  scen <- list(
    "horizon" = horizon,
    "cond_mat" = cond_mat
  )

  scen_store <- list(
    "fcast" = array(NA, c(n_save, horizon, M)),
    "setup" = scen, "variables" = object[["variables"]],
    "data" = object[["meta"]][["Y"]]
  )
  class(scen_store) <- "bvar_fcast"

  j <- 1
  for(i in seq_len(n_save)) {

    beta_comp <- get_beta_comp(beta[j, , ], K, M, lags)

    noshock_fcast <- compute_fcast(
      Y = Y, K = K, M = M, N = N, lags = lags,
      horizon = horizon,
      beta_comp = beta_comp,
      beta_const = beta[j, 1, ], sigma = sigma[j, , ],
      conditional = TRUE)

    ortho_irf <- irf_store[j, , , ]

    eta <- get_eta(cond_mat, noshock_fcast, ortho_irf, horizon, M)

    cond_fcast <- matrix(NA, horizon, M)

    for(h in seq_len(horizon)) {

      temp <- matrix(0, M , 1)

      for(k in seq_len(h)) {
        temp <- temp + ortho_irf[, (h - k + 1), ] %*% eta[ , k]
      }

      cond_fcast[h, ] <- noshock_fcast[h, ] + t(temp)
    }

    scen_store[["fcast"]][i, , ] <- cond_fcast

    j <- j + n_thin

  }

  # Prepare outputs -------------------------------------------------------

  # Apply confidence bands
  if(is.null(scen_store[["quants"]]) || !missing(conf_bands)) {
    scen_store <- if(!missing(conf_bands)) {
      predict.bvar_fcast(scen_store, conf_bands)
    } else {predict.bvar_fcast(scen_store, c(0.16))}
  }

  return(scen_store)
}



# Function to create matrix with conditions for conditional forecasts
get_cond_mat <- function(path, horizon,
                        cond_var, variables, M) {

  if(is.vector(path)) {

    if(length(path) > horizon) {stop("Conditions longer than forecast horizon.")}
    if(is.null(cond_var)) {stop("Please specify which variable to condition on.")}

    cond_mat <- matrix(NA, horizon, M)
    cond_var <- pos_vars(cond_var, variables, M)
    cond_mat[1:length(path), cond_var] <- path

    return(cond_mat)

  } else if(is.matrix(path)) {

    if(nrow(path) > horizon) {stop("Conditions longer than forecast horizon.")}
    if(nrow(path) > M) {stop("Path of conditions in wrong format.")}

    cond_mat <- path

    return(cond_mat)
  }

  stop("Path of conditions in wrong format.")

}

# Function to draw constrained shocks
get_eta <- function(cond_mat, noshock_fcast, ortho_irf, horizon, M) {

  v <- sum(!is.na(cond_mat))
  s <- M * horizon

  r <- c()
  R <- matrix(0, 0, s)

  for(i in seq_len(horizon)) {
    for(j in seq_len(M)) {

      if(!is.na(cond_mat[i, j])) {

        r <- c(r, (cond_mat[i, j] - noshock_fcast[i, j]))
        R <- rbind(R, c(rep(0, s)))
        for(k in 1:i) {
          R[nrow(R), ((k - 1) * M + 1):(k * M)] <- ortho_irf[j, (i - k + 1) , ]
        }

      }

    }
  }

  R_svd <- svd(R, nu = nrow(R), nv = ncol(R))

  U <- R_svd[["u"]]
  P_inv <- diag(1/R_svd[["d"]])
  V1 <- R_svd[["v"]][, 1:v]
  V2 <- R_svd[["v"]][, (v + 1):s]

  eta <- V1 %*% P_inv %*% t(U) %*% r + V2 %*% rnorm(s - v)

  eta <- matrix(eta, M, horizon)

  return(eta)
}
