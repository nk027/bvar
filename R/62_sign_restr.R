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
#' of certain shocks. Can be either 1, -1 or 0, depending on whether a
#' positive, a negative or no contemporaneous effect of a certain shock is
#' expected.
#' @param M Integer scalar. Columns of \emph{X}.
#' @param sign_lim Integer scalar. Maximum number of rotational matrices to
#' draw and check for fitting sign restrictions.
#'
#' @return Returns a matrix used as shock for computation of impulse responses,
#' that is identified via sign restrictions.
#'
#' @references
#'     Rubio-Ramirez, J. F., Waggoner, D. F., & Zha, T. (2010). Structural Vector Autoregressions: Theory of Identification and Algorithms for Inference. The Review of Economic Studies, 77, 665-696. \url{https://doi.org/10.1111/j.1467-937X.2009.00578.x}
#'
#' @importFrom stats rnorm
#'
#' @noRd
sign_restr <- function(sigma_chol, sign_restr, M, sign_lim = 10000) {

  counter <- 0
  sign_vec <- as.vector(sign_restr)
  restricted <- which(sign_vec != 0 | !is.na(sign_vec))

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
      stop("No matrix fitting the sign-restrictions found.")
    }
  }
}
