sign_restr <- function(sigma_chol, sign_restr, M) {

  counter <- 0
  restricted <- which(sign_restr != 0)

  while(TRUE) {
    counter <- counter + 1
    R_tilde <- matrix(rnorm(M ^ 2, 0, 1), M, M)
    qr_object <- qr(R_tilde)
    R <- qr.Q(qr_object)
    R <- R %*% diag((diag(R) > 0) - (diag(R) < 0))
    shock <- sigma_chol %*% R

    shock_vec <- as.vector(shock)
    shock_vec[which(shock_vec < 0)] <- -1
    shock_vec[which(shock_vec > 0)] <- 1

    if(all.equal(shock_vec[restricted], sign_restr[restricted])) break
    if(counter > 10000) stop("No matrix fitting the sign-restrictions found.")
  }

  return(shock)
}
