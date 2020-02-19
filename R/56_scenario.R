#' currently only helper functions for conditional forecasts


# Function to create matrix with conditions for conditional forecasts
get_cond_mat <- function(path, horizon,
                         cond_var, variables, M) {

  cond_mat <- matrix(NA, horizon, M)
  if(is.vector(path)) {
    cond_var <- pos_vars(cond_var, variables, M)
    cond_mat[1:length(path), cond_var] <- path
  } else {
    if(ncol(path) > M) {
      stop("Path includes to many variables.")
    }
    if(ncol(path) == M){
      cond_mat[seq_len(nrow(path)), ] <- path
    } else {
      cond_var <- pos_vars(cond_var, variables, M)
      cond_mat[1:nrow(path), cond_var] <- path
    }
  }

  return(cond_mat)
}



# Function to draw constrained shocks
get_eta <- function(cond_mat, noshock_fcast, ortho_irf, horizon, M) {

  v <- sum(!is.na(cond_mat))
  s <- M * horizon

  r <- c()
  R <- matrix(0, 0, s)

  for(i in seq_len(horizon)) {
    for(j in seq_len(M)) {
      if(!is.na(cond_mat[i, j])) {
        r <- c(r, (cond_mat[i, j] - noshock_fcast[i, j]))
        R <- rbind(R, c(rep(0, s)))
        for(k in 1:i) {
          R[nrow(R), ((k - 1) * M + 1):(k * M)] <- ortho_irf[j, (i - k + 1) , ]
        }
      }
    }
  }

  R_svd <- svd(R, nu = nrow(R), nv = ncol(R))

  U <- R_svd[["u"]]
  P_inv <- diag(1/R_svd[["d"]])
  V1 <- R_svd[["v"]][, 1:v]
  V2 <- R_svd[["v"]][, (v + 1):s]

  eta <- V1 %*% P_inv %*% t(U) %*% r + V2 %*% rnorm(s - v)

  eta <- matrix(eta, M, horizon)

  return(eta)
}
