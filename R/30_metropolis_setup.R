#' Metropolis-Hastings settings
#'
#' Function to provide settings for the Metropolis-Hastings step in
#' \code{\link{bvar}}. Options include scaling the inverse Hessian that
#' is used to draw parameter proposals and automatic scaling to
#' achieve a certain acceptance rate.
#'
#' @param scale_hess Numeric scalar or vector. Scaling parameter, determining
#' the range of hyperparameter draws. \strong{Should be calibrated so a
#' reasonable rate of acceptance is reached}. If provided as vector the length
#' must equal the number of hyperparameters (one per variable for \code{psi}).
#' @param adjust_acc Logical scalar. Whether or not to further scale the
#' variability of parameter draws during the burn-in phase.
#' @param acc_lower,acc_upper Numeric scalar. Lower (upper) bound of the target
#' acceptance rate. Required if \emph{adjust_acc} is set to \code{TRUE}.
#' @param acc_change Numeric scalar. Percent change applied to the Hessian
#' matrix. Required if \emph{adjust_acc} is set to \code{TRUE}.
#'
#' @param x An object of class \code{bv_metropolis}.
#' @param ... Not used
#'
#' @return Returns a named list of class \code{bv_metropolis} with options for
#' \code{\link{bvar}}.
#'
#' @keywords VAR BVAR MH metropolis-hastings acceptance mcmc
#'
#' @export
#'
#' @examples
#' # Only adjust the scale parameter
#' bv_mh(scale_hess = 10)
#'
#' # Turn on automatic scaling of the acceptance rate to [20%, 40%]
#' bv_mh(adjust_acc = TRUE, acc_lower = 0.2, acc_upper = 0.4)
#'
#' # Increase the rate of automatic scaling
#' bv_mh(adjust_acc = TRUE, acc_lower = 0.2, acc_upper = 0.4, acc_change = 0.1)
bv_metropolis <- function(
  scale_hess = 0.01,
  adjust_acc = FALSE,
  acc_lower = 0.25, acc_upper = 0.35,
  acc_change = 0.01) {

  scale_hess <- num_check(scale_hess, 1e-16, 1e16, "Problem with scale_hess.")

  if(adjust_acc) {
    acc_lower <- num_check(acc_lower, 0, 1 - 1e-16, "Problem with acc_lower.")
    acc_upper <- num_check(acc_upper, acc_lower, 1, "Problem with acc_upper.")
    acc_change <- num_check(acc_change, 1e-16, 1e16, "Problem with acc_change")
  }

  out <- list("scale_hess" = scale_hess,
              "adjust_acc" = adjust_acc,
              "acc_lower" = acc_lower, "acc_upper" = acc_upper,
              "acc_tighten" = 1 - acc_change, "acc_loosen" = 1 + acc_change)
  class(out) <- "bv_metropolis"

  return(out)
}


#' @rdname bv_metropolis
#' @export
bv_mh <- bv_metropolis
