#' Prior settings
#'
#' Helper function to set up parameters and informations on priors for
#' \code{\link{bvar}}. Used for adjusting the parameters treated as
#' hyperparameters, the Minnesota prior and adding various dummy priors.
#'
#' @param hyper Character vector. Used to specify the parameters to be treated
#' as hyperparameters. Can also be set to "auto" or "full" for
#' an automatic / full subset. Other allowed values are the Minnesota prior's
#' parameters "lambda", "alpha" and "psi" as well as the names of possible
#' dummy priors.
#' @param mn List of class "bv_minnesota". Options for the Minnesota prior,
#' set via \code{\link{bv_mn}}.
#' @param ... Optional lists of class "bv_dummy" with options for possible
#' dummy priors. Must be assigned a name in the function call. Created with
#' \code{\link{bv_dummy}}
#'
#' @return Returns a named list of class bv_priors with options for
#' \code{\link{bvar}}.
#' @export
#'
#' @seealso \code{\link{bv_mn}}; \code{\link{bv_dummy}}
#'
#' @examples
#' # Extending hyperparameters
#' bv_priors(c("lambda", "alpha", "psi"))
#'
#' # Adding already constructed dummy priors
#' # bv_priors(hyper = "auto", "soc" = soc_prior, "sur" = sur_prior)
bv_priors <- function(
  hyper = "auto",
  mn = bv_mn(bv_lambda(0.2, 0.4, 0.0001, 5),
             bv_alpha(2, 0.1, 0.5),
             var = 1e07),
  ...) {

  # Check inputs ------------------------------------------------------------

  if(!is.null(mn) && !inherits(mn, "bv_minnesota")) {
    stop("Please use `bv_mn()` to set the minnesota prior.")
  }
  dots <- list(...)
  if(!all(vapply(dots, inherits, TRUE, "bv_dummy"))) {
    stop("Please use `bv_dummy()` to set dummy priors.")
  }
  if(hyper[[1]] == "auto") {
    hyper <- c(if(!is.null(mn)) {"lambda"}, names(dots))
  } else {
    full <- c(if(!is.null(mn)) {c("lambda", "alpha", "psi")}, names(dots))
    if(hyper[[1]] == "full") {
      hyper <- full
    } else {
      if(!all(hyper %in% full)) {stop("Hyperprior not found.")}
    }
  }


  # Output ------------------------------------------------------------------

  out <- if(!is.null(mn)) {
    list(hyper = hyper, lambda = mn[["lambda"]], alpha = mn[["alpha"]],
         psi = mn[["psi"]], var = mn[["var"]], b = mn[["b"]], ...)
  } else {
    list(hyper = hyper, ...)
  }
  class(out) <- "bv_priors"

  return(out)
}
