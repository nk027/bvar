
#' Impulse response and forecast error methods for Bayesian VARs
#'
#' Retrieves / calculates impulse response functions (IRFs) and/or forecast
#' error variance decompositions (FEVDs) for Bayesian VARs generated via
#' \code{\link{bvar}}. If the object is already present and no settings are
#' supplied it is simply retrieved, otherwise it will be calculated ex-post.
#' Note that FEVDs require the presence / calculation of IRFs.
#' To store the results you may want to assign the output using the setter
#' function (\code{irf(x) <- irf(x)}). May also be used to update
#' confidence bands.
#'
#' @param x,object A \code{bvar} object, obtained from \code{\link{bvar}}.
#' Summary and print methods take in a \code{bvar_irf} / \code{bvar_fevd}
#' object.
#' @param ... A \code{bv_irf} object or arguments to be fed into
#' \code{\link{bv_irf}}. Contains settings for the IRFs / FEVDs.
#' @param n_thin Integer scalar. Every \emph{n_thin}'th draw in \emph{x} is used
#' to calculate, others are dropped.
#' @param vars_impulse,vars_response Optional numeric or character vector.
#' Used to subset the summary method's outputs to certain variables by position
#' or name (must be available). Defaults to \code{NULL}, i.e. all variables.
#' @param value A \code{bvar_irf} object to assign.
#' @inheritParams predict.bvar
#'
#' @return Returns a list of class \code{bvar_irf} including IRFs and optionally
#' FEVDs at desired confidence bands. The \code{fevd} method only returns a
#' the nested \code{bvar_fevd} object.
#' The summary method returns a numeric array of impulse responses at the
#' specified confidence bands.
#'
#' @seealso \code{\link{plot.bvar_irf}}; \code{\link{bv_irf}}
#'
#' @keywords BVAR irf fevd analysis
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Access a subset of the fred_qd dataset
#' data <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]
#' # Transform it to be stationary
#' data <- fred_transform(data, codes = c(5, 5, 1), lag = 4)
#'
#' # Estimate a BVAR using one lag, default settings and very few draws
#' x <- bvar(data, lags = 1, n_draw = 600L, n_burn = 100L, verbose = FALSE)
#'
#' # Compute + store IRF with a longer horizon, no identification and thinning
#' irf(x) <- irf(x, bv_irf(horizon = 24L, identification = FALSE), n_thin = 5L)
#'
#' # Update the confidence bands of the IRFs
#' irf(x, conf_bands = c(0.01, 0.05, 0.1))
#'
#' # Recalculate with sign restrictions provided via the ellipsis
#' irf(x, sign_restr = matrix(c(1, NA, NA, -1, 1, -1, -1, 1, 1), nrow = 3))
#'
#' # Recalculate with zero and sign restrictions provided via the ellipsis
#' irf(x, sign_restr = matrix(c(1, 0, 1, NA, 1, 1, -1, -1, 1), nrow = 3))
#'
#' # Calculate the forecast error variance decomposition
#' fevd(x)
#'
#' # Get a summary of the saved impulse response function
#' summary(x)
#'
#' # Limit the summary to responses of variable #2
#' summary(x, vars_response = 2L)
#' }
irf.bvar <- function(x, ..., conf_bands, n_thin = 1L) {

  dots <- list(...)
  irf_store <- x[["irf"]]


  # Calculate impulse responses -----

  if(is.null(irf_store) || length(dots) != 0L) {

    # Setup ---

    irf <- if(length(dots) > 0 && inherits(dots[[1]], "bv_irf")) {
      dots[[1]]
    } else {bv_irf(...)}

    n_pres <- x[["meta"]][["n_save"]]
    n_thin <- int_check(n_thin, min = 1, max = (n_pres / 10),
      "Issue with n_thin. Maximum allowed is n_save / 10.")
    n_save <- int_check((n_pres / n_thin), min = 1)

    Y <- x[["meta"]][["Y"]]
    N <- x[["meta"]][["N"]]
    K <- x[["meta"]][["K"]]
    M <- x[["meta"]][["M"]]
    lags <- x[["meta"]][["lags"]]
    beta <- x[["beta"]]
    sigma <- x[["sigma"]]

    # Check sign restrictions
    if(!is.null(irf[["sign_restr"]]) && length(irf[["sign_restr"]]) != M ^ 2) {
      stop("Dimensions of provided sign restrictions do not fit the data.")
    }
    if(!is.null(irf[["zero_restr"]]) && length(irf[["zero_restr"]]) != M ^ 2) {
      stop("Dimensions of provided zero and sign restrictions ",
           "do not fit the data.")
    }

    # Sampling ---

    irf_store <- structure(list(
      "irf" = array(NA, c(n_save, M, irf[["horizon"]], M)),
      "fevd" = if(irf[["fevd"]]) {
        structure(
          list("fevd" = array(NA, c(n_save, M, irf[["horizon"]], M)),
            "variables" = x[["variables"]]), class = "bvar_fevd")
      } else {NULL}, "setup" = irf, "variables" = x[["variables"]]),
      class = "bvar_irf")

    j <- 1
    for(i in seq_len(n_save)) {
      beta_comp <- get_beta_comp(beta[j, , ], K, M, lags)
      irf_comp  <- compute_irf(
        beta_comp = beta_comp, sigma = sigma[j, , ], M = M, lags = lags,
        horizon = irf[["horizon"]], identification = irf[["identification"]],
        sign_restr = irf[["sign_restr"]], zero = irf[["zero"]],
        sign_lim = irf[["sign_lim"]])
      irf_store[["irf"]][i, , , ] <- irf_comp

      if(irf[["fevd"]]) { # Forecast error variance decomposition
        irf_store[["fevd"]][["fevd"]][i, , , ] <- compute_fevd(
          irf_comp = irf_comp, M = M, horizon = irf[["horizon"]])
      }
      j <- j + n_thin
    }
  } # End new impulse responses

  if(is.null(irf_store[["quants"]]) || !missing(conf_bands)) {
    irf_store <- if(!missing(conf_bands)) {
      irf.bvar_irf(irf_store, conf_bands)
    } else {irf.bvar_irf(irf_store, c(0.16))}
  }

  if(irf_store[["setup"]][["fevd"]]) {
    if(is.null(irf_store[["fevd"]][["quants"]]) || !missing(conf_bands)) {
      irf_store[["fevd"]] <- if(!missing(conf_bands)) {
        fevd.bvar_irf(irf_store, conf_bands)
      } else {fevd.bvar_irf(irf_store, c(0.16))}
    }
  }



  return(irf_store)
}


#' @noRd
#' @export
`irf<-.bvar` <- function(x, value) {

  if(!inherits(x, "bvar")) {stop("Please use a `bvar` object.")}
  if(!inherits(value, "bvar_irf")) {
    stop("Please provide a `bvar_irf` object to assign.")
  }

  x[["irf"]] <- value

  return(x)
}


#' @noRd
#' @export
#'
#' @importFrom stats quantile
irf.bvar_irf <- function(x, conf_bands, ...) {

  if(!missing(conf_bands)) {
    quantiles <- quantile_check(conf_bands)
    x[["quants"]] <- apply(x[["irf"]], c(2, 3, 4), quantile, quantiles)
  }

  return(x)
}


#' @rdname irf.bvar
#' @export
fevd.bvar <- function(x, ..., conf_bands, n_thin = 1L) {

  dots <- list(...)
  irf_store <- x[["irf"]]
  vars <- x[["variables"]]
  if(is.null(vars)) {vars <- paste0("var", 1:x[["meta"]][["M"]])}

  if(is.null(irf_store[["fevd"]]) || length(dots) != 0L) {
    irf <- if(length(dots) > 0 && inherits(dots[[1]], "bv_irf")) {
      dots[[1]]
    } else {bv_irf(...)}
    irf[["fevd"]] <- TRUE
    irf_store <- irf.bvar(x, irf, n_thin = n_thin) # Recalculate
  }

  fevd_store <- fevd.bvar_irf(irf_store, conf_bands = conf_bands)

  return(fevd_store)
}


#' @noRd
#' @export
`fevd<-.bvar` <- function(x, value) {

  if(!inherits(x, "bvar")) {stop("Please use a `bvar` object.")}
  if(!inherits(value, "bvar_fevd")) {
    stop("Please provide a `bvar_fevd` object to assign.")
  }

  x[["fevd"]] <- value

  return(x)
}


#' @noRd
#' @export
fevd.bvar_irf <- function(x, conf_bands, ...) {

  if(is.null(x[["fevd"]])) {
    x[["fevd"]] <- structure(list("fevd" = array(NA, dim(x[["irf"]])),
      "variables" = x[["variables"]]), class = "bvar_fevd")
    n_save <- dim(x[["irf"]])[1]
    M <- dim(x[["irf"]])[2]
    horizon <- dim(x[["irf"]])[3]
    for(i in seq_len(n_save)) {
      irf_comp <- x[["irf"]][i, , , ]
      x[["fevd"]][["fevd"]][i, , , ] <- compute_fevd(irf_comp = irf_comp,
        M = M, horizon = horizon)
    }
  }

  fevd_store <- x[["fevd"]]

  if(is.null(fevd_store[["quants"]]) || !missing(conf_bands)) {
    fevd_store <- if(!missing(conf_bands)) {
      fevd.bvar_fevd(fevd_store, conf_bands)
    } else {fevd.bvar_fevd(fevd_store, c(0.16))}
  }

  return(fevd_store)
}


#' @noRd
#' @export
#'
#' @importFrom stats quantile
fevd.bvar_fevd <- function(x, conf_bands, ...) {

  if(!missing(conf_bands)) {
    quantiles <- quantile_check(conf_bands)
    x[["quants"]] <- apply(x[["fevd"]], c(2, 3, 4), quantile, quantiles)
    # Make 'em sum to 1 (breaks with quantiles) and keep dimension ordering
    apply_vec <- if(length(quantiles) > 1) {c(1, 2, 3)} else {c(1, 2)}
    aperm_vec <- if(length(quantiles) > 1) {c(2, 3, 4, 1)} else {c(2, 3, 1)}
    x[["quants"]] <- apply(x[["quants"]], apply_vec, function(x) {
      x / sum(x)})
    x[["quants"]] <- aperm(x[["quants"]], aperm_vec)
  }

  return(x)
}


#' @rdname irf.bvar
#' @export
irf <- function(x, ...) {UseMethod("irf", x)}


#' @noRd
irf.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @rdname irf.bvar
#' @export
`irf<-` <- function(x, value) {UseMethod("irf<-", x)}


#' @rdname irf.bvar
#' @export
fevd <- function(x, ...) {UseMethod("fevd", x)}


#' @noRd
fevd.default <- function(x, ...) {
  stop("No methods for class ", paste0(class(x), collapse = " / "), " found.")
}


#' @rdname irf.bvar
#' @export
`fevd<-` <- function(x, value) {UseMethod("fevd<-", x)}
