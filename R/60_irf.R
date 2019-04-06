
irf_draw <- function(beta_comp, sigma, sigma_chol, M, lags,
                     horizon, identification, sign_restr = NULL, fevd) {

  # Identification
  shock <- if(identification) {
    if(is.null(irf_signs)) sigma_chol else sign_restr(sigma_chol, sign_restr, M)
  } else {
    sigma
  }

  # IRF
  irf_comp <- array(0, c(M * lags, horizon, M * lags))
  irf_comp[1:M, 1, 1:M] <- shock
  for(i in 2:horizon) {
    irf_comp[, i, ] <- irf_comp[, i - 1, ] %*% t(beta_comp)
  }
  irf_comp <- irf_comp[1:M, , 1:M]
  out <- list("irf" = irf_comp)

  # FEVD
  if(fevd) {
    fevd_comp <- aperm(apply(irf_comp * irf_comp, c(1, 3), cumsum), c(2, 3, 1))
    accm <- matrix(0, M, M)
    for(i in 1:horizon) {
      accm <- accm + irf_comp[, i, ] %*% t(irf_comp[, i, ])
      denm <- matrix((diag(accm)), M, M)
      fevd_comp[, , i] <- fevd_comp[, , i] / denm
    }
    out[["fevd"]] <- apply(fevd_comp, c(1, 2), mean, na.rm = TRUE)
  }

  return(out)
}
