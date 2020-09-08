#' Sign restriction algorithm
#'
#' Implements the algorithm by Rubio-Ramirez et al. (2010) in order to find
#' suitable matrices containing sign restrictions for identification purposes.
#' Called by \code{\link{compute_irf}} and throws an error if no suitable
#' restrictions can be found.
#'
#' @param sigma_chol Numeric matrix. Lower part of the Cholesky decomposition
#' of \emph{sigma}. Calculated as \code{t(chol(sigma))}.
#' @param sign_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1} or \eqn{-1} depending on
#' whether a positive or a negative effect of a certain shock is expected.
#' Elements set to \eqn{NA} indicate that there are no particular expectations
#' for the contemporaneous effects.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param sign_lim Integer scalar. Maximum number of rotational matrices to
#' draw and check for fitting sign restrictions.
#'
#' @return Returns a matrix used as shock for computation of impulse responses
#' that is identified via sign restrictions.
#'
#' @references
#'   Rubio-Ramirez, J. F. and Waggoner, D. F. and Zha, T. (2010) Structural
#'   Vector Autoregressions: Theory of Identification and Algorithms for
#'   Inference. \emph{The Review of Economic Studies}, \bold{77}, 665-696,
#'   \url{https://doi.org/10.1111/j.1467-937X.2009.00578.x}.
#'
#' @importFrom stats rnorm
#'
#' @noRd
sign_restr <- function(sigma_chol, sign_restr, M, sign_lim = 10000) {

  counter <- 0
  sign_vec <- as.vector(sign_restr)
  restricted <- which(!is.na(sign_vec))
  # restricted <- which(sign_vec != 0)

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

    if(identical(shock_vec[restricted], sign_vec[restricted])) {return(shock)}
    if(counter > sign_lim) {
      stop("No matrix fitting the sign restrictions found.")
    }
  }
}


#' Zero-sign restriction algorithm
#'
#' Implements the algorithm by Arias et al. (2018) in order to find
#' suitable matrices containing zero and sign restrictions for identification
#' purposes. Called by \code{\link{compute_irf}} and throws an error if no
#' suitable restrictions can be found.
#'
#' @param sigma_chol Numeric matrix. Lower part of the Cholesky decomposition
#' of \emph{sigma}. Calculated as \code{t(chol(sigma))}.
#' @param zero_restr Numeric matrix. Elements inform about expected impacts
#' of certain shocks. Can be either \eqn{1}, \eqn{-1} or \eqn{0} depending
#' on whether a positive, a negative or no contemporaneous effect of a
#' certain shock is expected. Elements set to \eqn{NA} indicate that there are
#' no particular expectations for the contemporaneous effects.
#' @param M Integer scalar. Columns of \emph{Y}.
#' @param sign_lim Integer scalar. Maximum number of tries to find a suitable
#' matrix that fits zero and sign restrictions.
#'
#' @return Returns a matrix used as shock for computation of impulse responses
#' that is identified via zero and sign restrictions.
#'
#' @references
#'   Arias, J.E. and Rubio-Ramirez, J. F. and Waggoner, D. F. (2018)
#'   Inference Based on Structural Vector Autoregressions Identifiied with
#'   Sign and Zero Restrictions: Theory and Applications.
#'   \emph{Econometrica}, \bold{86}, 2, 685-720,
#'   \url{https://doi.org/10.3982/ECTA14468}.
#'
#' @importFrom stats rnorm
#'
#' @noRd
zero_restr <- function(sigma_chol, zero_restr, M, sign_lim = 10000) {

  counter <- 0
  sign_vec <- as.vector(zero_restr)
  restricted <- which(!is.na(sign_vec) & sign_vec != 0)

  while(TRUE) {
    counter <- counter + 1
    Q <- matrix(0, M, M)
    for(i in seq_len(M)) {
      slct_row <- which(zero_restr[ , i] == 0)
      R <- rbind(sigma_chol[slct_row, ], Q[seq_len(i - 1), ])

      qr.temp <- qr(t(R))
      qr.rank <- qr.temp[[2]]
      if(qr.rank == 0) {
        set <- seq_len(M)
      } else {
        set <- -seq_len(qr.rank)
      }
      N_i <- qr.Q(qr.temp, complete = TRUE)[, set, drop = FALSE]
      x_i <- rnorm(M, 0, 1)
      q_i <- N_i %*% (t(N_i) %*% x_i / norm(t(N_i) %*% x_i, type = "2"))
      Q[i, ] <- q_i
    }

    shock <- sigma_chol %*% t(Q)
    shock[abs(shock) < 1e-10] <- 0

    shock_vec <- as.vector(shock)
    shock_vec[which(shock_vec < 0)] <- -1
    shock_vec[which(shock_vec > 0)] <- 1

    if(identical(shock_vec[restricted], sign_vec[restricted])) {return(shock)}
    if(counter > sign_lim) {
      stop("No matrix fitting the sign restrictions found.")
    }
  }
}
