bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(!is.logical(c(identification, fevd))){
    stop("Parameter(s) are not provided as the correct type.")
  }

  if(!is.null(sign_restr) && !all(sign_restr %in% c(-1, 0, 1))){
    stop("Sign restrictions misspecified.")
  }

  out <- list("horizon" = horizon, "fevd" = fevd,
              "identification" = identification, "sign_restr" = sign_restr)
  class(out) <- "bv_irf"

  return(out)
}


