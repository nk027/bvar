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
      stop("Path of conditions includes too many variables.")
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

# Function to draw conditional forecasts
get_cond_fcast <- function(cond_mat, noshock_fcast, ortho_irf, horizon, M) {

  cond_fcast <- matrix(NA, horizon, M)

  # First get constrained shocks
  v <- sum(!is.na(cond_mat))
  s <- M * horizon

  r <- c()
  R <- matrix(0, 0, s)

  # Construct R and r
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
  # draw constrained shocks
  eta <- V1 %*% P_inv %*% t(U) %*% r + V2 %*% rnorm(s - v)
  # reshape them
  eta <- matrix(eta, M, horizon)

  for(h in seq_len(fcast[["horizon"]])) {
    temp <- matrix(0, M, 1)
    for(k in seq_len(h)) {
      temp <- temp + ortho_irf[, (h - k + 1), ] %*% eta[ , k]
    }
    cond_fcast[h, ] <- noshock_fcast[h, ] + t(temp)
  }

  return(cond_fcast)
}


