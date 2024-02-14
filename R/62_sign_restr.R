
#' Sign restriction algorithm
#'
#' Implements the algorithms by Rubio-Ramirez, Waggoner and Zha (2010) and
#' Arias, Rubio-Ramirez and Waggoner (2018) in order to find suitable
#  sign and/or zero restricted matrices for identification purposes.
#' Called by \code{\link{compute_irf}} and throws an error if no suitable
#' restrictions can be found. Called by \code{\link{compute_irf}} and throws
#' an error if no suitable restrictions are found after \code{sign_lim} draws.
#'
#' @param sigma_chol Numeric matrix. Lower part of the Cholesky decomposition
#' of \emph{sigma}. Calculated as \code{t(chol(sigma))}.
#' @param sign_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1}, \eqn{-1} or \eqn{0} depending
#' on whether a positive, a negative or no contemporaneous effect of a
#' certain shock is expected. Elements set to \eqn{NA} indicate that there are
#' no particular expectations for the contemporaneous effects.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param zero Logical scalar. Whether to impose zero and sign restrictions,
#' following Arias, Rubio-Ramirez and Waggoner (2018).
#' @param sign_lim Integer scalar. Maximum number (approximately) of rotational
#' matrices to draw and check for fitting sign restrictions.
#'
#' @return Returns a shock matrix for the computation of impulse responses
#' that is identified via sign and/or zero restrictions.
#'
#' @references
#'   Rubio-Ramirez, J. F. and Waggoner, D. F. and Zha, T. (2010) Structural
#'   Vector Autoregressions: Theory of Identification and Algorithms for
#'   Inference. \emph{The Review of Economic Studies}, \bold{77}, 665-696,
#'   \url{https://doi.org/10.1111/j.1467-937X.2009.00578.x}.
#'   Arias, J.E. and Rubio-Ramirez, J. F. and Waggoner, D. F. (2018)
#'   Inference Based on Structural Vector Autoregressions Identifiied with
#'   Sign and Zero Restrictions: Theory and Applications.
#'   \emph{Econometrica}, \bold{86}, 2, 685-720,
#'   \url{https://doi.org/10.3982/ECTA14468}.
#'
#' @importFrom stats rnorm
#'
#' @noRd
sign_restr <- function(sigma_chol,
  sign_restr, M, zero = FALSE, sign_lim = 1000) {

  counter_outer <- 0L
  while(TRUE) { # Search for a shock until we exceed the number of tries
    if(counter_outer > min(sign_lim^.5)) { # Minimum of 10 tries (100^.5)
      stop(paste0("No matrix fitting the sign restrictions found after ",
        sign_lim, " tries. Consider increasing the limit via the",
        "`sign_lim` argument of `bv_irf()` or adapting the restrictions."))
    }
    counter_outer <- counter_outer + 1L

    Q <- matrix(0, M, M) # Shock matrix
    pos_check <- logical(M) # Vector that indicates suitable shocks

    i <- 1L
    counter_inner <- 0L
    while(!all(pos_check)) {
      if(counter_inner > min(sign_lim^.6)) break
      counter_inner <- counter_inner + 1L # Minimum of 15 tries (100^.6)

      # Draw and check a shock
      q_i <- draw_qi(sigma_chol, sign_restr, M, i = i, zero = zero, Q = Q)
      sign_check <- check_qi(q_i, sigma_chol, sr_i = sign_restr[, i])

      if(sign_check != 0L) { # The signs are correct
        pos_check[i] <- TRUE
        Q[, i] <- q_i * sign_check # Keep or flip the sign of the shock
        i <- i + 1L
      }
    }

    if(all(pos_check)) {return(sigma_chol %*% Q)}
  }
}

#' @noRd
draw_qi <- function(sigma_chol, sign_restr, M, i, zero = FALSE, Q) {

  if(isTRUE(zero)) { # Zero-sign-restrictions
    Q <- t(Q)
    sel_row <- which(sign_restr[, i] == 0)
    R <- rbind(sigma_chol[sel_row, ], Q[seq_len(i - 1L), ])
    qr_object <- qr(t(R))
    qr_rank <- qr_object[["rank"]]
    set <- if(qr_rank == 0) {seq_len(M)} else {-seq_len(qr_rank)}
    N_i <- qr.Q(qr_object, complete = TRUE)[, set, drop = FALSE]
    N_stdn <- crossprod(N_i, rnorm(M, 0, 1))
    q_i <- N_i %*% (N_stdn / norm(N_stdn, type = "2"))
  } else { # Pure sign-restrictions
    if(i == 1) {
      x <- rnorm(M, 0, 1)
      q_i <- x / norm(x, type = "2")
    } else {
      x <- rnorm(M, 0, 1)
      QQ <- diag(M) - tcrossprod(Q)
      q_i <- QQ %*% x / norm(QQ %*% x, type = "2")
    }
  }

  return(q_i)
}

#' @noRd
check_qi <- function(q_i, sigma_chol, sr_i) {

  restricted <- which(!is.na(sr_i) & sr_i != 0)
  shock_vec <- sigma_chol %*% q_i
  shock_vec <- sign(shock_vec)

  # Return 1L for a fit, -1L for a fit with flipped signs, and 0L for a failure
  if(identical(shock_vec[restricted], sr_i[restricted])) {
    return(1L)
  } else if (identical(-shock_vec[restricted], sr_i[restricted])) {
    return(-1L)
  } else {
    return(0L)
  }
}
