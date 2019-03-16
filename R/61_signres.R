sign_res <- function(sigma_draw, irf_signs, M){

  counter <- 0
  restrictions <- FALSE
  restricted <- which(irf_signs!=0)

  while(!restrictions) {
    counter <- counter + 1
    R_tilde <- matrix(rnorm(M ^ 2, 0, 1), M, M)
    qr_object <- qr(R_tilde)
    R <- qr.Q(qr_object)
    R <- R %*% diag((diag(R) > 0) - (diag(R) < 0))
    shock <- t(chol(sigma_draw)) %*% R

    shock_vec <- as.vector(shock)
    shock_vec[which(shock_vec < 0)] <- -1
    shock_vec[which(shock_vec > 0)] <- 1

    if(all.equal(shock_vec[restricted], irf_signs[restricted])) {
      restrictions <- TRUE
      }
    if(counter > 10000) {
      stop("No matrix fitting the sign-restrictions found.")
    }
  }

  return(shock)
}
