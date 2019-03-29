bv_irf <- function(
  irf_hor = 12,
  irf_id = TRUE,
  irf_signs = NULL,
  fevd = FALSE) {

  if(!is.numeric(irf_hor) || irf_hor < 1){
    stop("IRF horizon misspecified.")
  }

  if(!is.logical(c(irf_id, fevd))){
    stop("irf_id and fevd have to be logical.")
  }

  if(!is.null(irf_signs) && !all(irf_signs %in% c(-1, 0, 1))){
    stop("Sign restrictions misspecified.")
  }


  out <- list("irf_hor" = irf_hor,
              "irf_id" = irf_id,
              "irf_signs" = irf_signs,
              "fevd" = fevd)
  class(out) <- "bv_irf"

  return(out)
}
