
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
#' @param sign_lim Integer scalar. Maximum number of rotational matrices to
#' draw and check for fitting sign restrictions.
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
  sign_restr, M, zero = FALSE, sign_lim = 10000) {

  counter <- 0
  sign_vec <- as.vector(sign_restr)
  restricted <- which(!is.na(sign_vec) & sign_vec != 0)

  while(TRUE) {
    counter <- counter + 1
    Q <- draw_Q(sigma_chol, sign_restr, M, zero = zero)
    shock <- sigma_chol %*% Q
    shock[abs(shock) < 1e-12] <- 0

    shock_vec <- as.vector(shock)
    shock_vec[which(shock_vec < 0)] <- -1
    shock_vec[which(shock_vec > 0)] <- 1

    if(identical(shock_vec[restricted], sign_vec[restricted])) {return(shock)}
    if(counter > sign_lim) {
      stop("No matrix fitting the sign restrictions found.")
    }
  }
}


#' @noRd
draw_Q <- function(sigma_chol, sign_restr, M, zero = FALSE) {

  if(!zero) { # Sign restricted
    qr_object <- qr(matrix(rnorm(M ^ 2, 0, 1), M, M))
    Q <- qr.Q(qr_object)
  } else { # Zero restricted
    Q <- matrix(0, M, M)
    for(i in seq_len(M)) { # Build up Q
      slct_row <- which(sign_restr[, i] == 0)
      R <- rbind(sigma_chol[slct_row, ], Q[seq_len(i - 1), ])
      qr_object <- qr(t(R))
      qr_rank <- qr_object[["rank"]]
      set <- if(qr_rank == 0) {seq_len(M)} else {-seq_len(qr_rank)}
      N_i <- qr.Q(qr_object, complete = TRUE)[, set, drop = FALSE]
      N_stdn <- crossprod(N_i, rnorm(M, 0, 1))
      q_i <- N_i %*% (N_stdn / norm(N_stdn, type = "2"))
      Q[i, ] <- q_i
    }
    Q <- t(Q)
  }

  Q <- Q %*% diag((diag(Q) > 0) - (diag(Q) < 0)) # Positive

  return(Q)
}
