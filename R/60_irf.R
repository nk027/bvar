irf_draw <- function(beta_comp, sigma, sigma_chol, M, lags,
                     horizon, identification, sign_restr, fevd) {

  # Identification
  shock <- if(identification) {
    if(is.null(sign_restr)) {
      sigma_chol
    } else {sign_restr(sigma_chol, sign_restr, M)}
  } else {sigma}

  # IRF
  irf_comp <- array(0, c(M * lags, horizon, M * lags))
  irf_comp[1:M, 1, 1:M] <- shock
  for(i in 2:horizon) {
    irf_comp[, i, ] <- irf_comp[, i - 1, ] %*% t(beta_comp)
  }
  irf_comp <- irf_comp[1:M, , 1:M]
  out <- list("irf" = irf_comp)

  # FEVD
  if(fevd) out[["fevd"]] <- fevd(irf_comp, M, horizon)

  return(out)
}
