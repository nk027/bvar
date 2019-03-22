
bv_irf <- function(beta_comp, sigma_draw,
                   M = NULL, lags = NULL,
                   irf_hor = 12, irf_id, irf_signs = NULL, fevd){

  if(irf_id){
    if(is.null(irf_signs)){
      shock <- t(chol(sigma_draw))
    }else{
      shock <- sign_res(sigma_draw, irf_signs, M)
    }
  }else{
    shock <- sigma_draw
  }


  # IRF
  irf_comp <- array(0, c(M * lags, irf_hor, M * lags))
  irf_comp[1:M, 1, 1:M] <- shock
  for(j in 2:irf_hor) {
    irf_comp[, j, ] <- irf_comp[, j - 1, ] %*% t(beta_comp)
  }
  out <- list("irf_comp" = irf_comp)

  # FEVD
  if(fevd){
    fevd_comp<- apply(irf_comp[1:M, , 1:M] * irf_comp[1:M, , 1:M],
                       c(1, 3), cumsum)
    fevd_comp <- aperm(fevd_comp, c(2, 3, 1))
    accm <- matrix(0, M, M)
    for (i in 1:irf_hor) {
      accm <- accm + irf_comp[1:M, i, 1:M] %*% t(irf_comp[1:M, i, 1:M])
      denm <- matrix((diag(accm)), M, M)
      fevd_comp[, , i] <- fevd_comp[, , i] / denm
    }
    out[["fevd_comp"]] <- fevd_comp
  }



  return(out)

}
