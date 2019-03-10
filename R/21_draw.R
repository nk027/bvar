bv_draw <- function(draw_list) {

  S_post <- draw_list$psi + draw_list$sse +
    t(draw_list$beta_hat - draw_list$b) %*%
    draw_list$omega_inv %*%
    (draw_list$beta_hat - draw_list$b)
  S_eig <- eigen(S_post)
  S_inv <- S_eig$vectors %*% diag(1 / abs(S_eig$values)) %*% t(S_eig$vectors)
  eta <- MASS::mvrnorm(n = (draw_list$N_ml + draw_list$M_ml + 2),
                       mu = rep(0, draw_list$M_ml),
                       Sigma = S_inv)
  sigma_draw <- solve(crossprod(eta)) %*% diag(draw_list$M_ml)
  sigma_chol <- t(chol(sigma_draw))
  beta_draw <- draw_list$beta_hat +
    t(MASS::mvrnorm(n = draw_list$M_ml,
                    mu = rep(0, (1 + draw_list$M_ml * draw_list$lags_ml)),
                    Sigma = solve(crossprod(draw_list$X_ml) +
                                    draw_list$omega_inv) %*%
                      diag(1 + draw_list$M_ml * draw_list$lags_ml))) %*%
    sigma_chol

  out <- list("beta_draw" = beta_draw,
              "sigma_draw" = sigma_draw,
              "sigma_chol" = sigma_chol)
  return(out)
}
