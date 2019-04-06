fevd <- function(irf_comp, M, horizon) {

  fevd_comp <- aperm(apply(irf_comp * irf_comp, c(1, 3), cumsum), c(2, 3, 1))
  accm <- matrix(0, M, M)
  for(i in 1:horizon) {
    accm <- accm + irf_comp[, i, ] %*% t(irf_comp[, i, ])
    denm <- matrix((diag(accm)), M, M)
    fevd_comp[, , i] <- fevd_comp[, , i] / denm
  }

  return(apply(fevd_comp, c(1, 2), mean, na.rm = TRUE))
}
