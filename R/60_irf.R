bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon.")

  if(!is.logical(c(identification, fevd))){
    stop("Parameter(s) are not provided as the correct type.")
  }

  if(!is.null(sign_restr) && !all(sign_restr %in% c(-1, 0, 1)) &&
     sqrt(length(sign_restr)) %% 1 == 0) {
    stop("Issue with sign restrictions.")
  }

  if(is.vector(sign_restr)) {
    sign_restr <- matrix(sign_restr, nrow = sqrt(length(sign_restr)))
  }

  out <- list("horizon" = horizon, "fevd" = fevd,
              "identification" = identification, "sign_restr" = sign_restr)
  class(out) <- "bv_irf"

  return(out)
}
