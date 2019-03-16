bv_metropolis <- function(
  scale_hess = 0.01,
  adjust_acc = FALSE,
  acc_lower = 0.25, acc_upper = 0.35,
  acc_change = 0.01) {

  if(!is.numeric(c(scale_hess, acc_lower, acc_upper, acc_change)) ||
     !is.logical(adjust_acc)) {
    stop("Parameter(s) are not provided as the correct type.")
  }
  if(acc_lower >= acc_upper) {
    stop("Acceptance boundaries misspecified.")
  }

  acc_tighten <- 1 - acc_change
  acc_loosen <- 1 + acc_change

  out <- list("scale_hess" = scale_hess,
              "adjust_acc" = adjust_acc,
              "acc_lower" = acc_lower, "acc_upper" = acc_upper,
              "acc_tighten" = acc_tighten, "acc_loosen" = acc_loosen)

  return(out)
}
