
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

  outer_counter <- 0
  while(TRUE) {
    outer_counter <- outer_counter + 1
    Q <- matrix(0, M, M)
    pos_check <- numeric(M)
    i <- 1
    inner_counter <- 0
    while(sum(pos_check) != M) {
      inner_counter <- inner_counter + 1
      q_i <- draw_qi(sigma_chol, sign_restr, M, iter = i, zero = zero, Q)
      chk_signs <- check_qi(q_i, sigma_chol, sign_restr, M, iter = i, zero = zero, pos_check)

      if(is.numeric(chk_signs)) {
        idx <- chk_signs[2]
        pos_check[idx] <- 1
        Q[ , idx] <- q_i * chk_signs[1]
        i <- i + 1
      }
      if(inner_counter > round(sign_lim / 10, 0)) {
        break
      }
    }

    if(is.numeric(chk_signs)) {
      shock <- sigma_chol %*% Q
      return(shock)
    }
    if(outer_counter > sign_lim) {
      stop("No matrix fitting the sign restrictions found.")
    }
  }
}

#' @noRd
draw_qi <- function(sigma_chol, sign_restr, M, iter, zero = FALSE, Q) {
  if(!zero) {
    if(iter == 1) {
      x <- rnorm(M, 0, 1)
      q_i <- x / norm(x, type = "2")
    } else {
      x <- rnorm(M, 0, 1)
      QQ <- (diag(M) - Q %*% t(Q))
      q_i <- QQ %*% x / norm(QQ %*% x, type = "2")
    }
  } else {
    slct_row <- which(sign_restr[, iter] == 0)
    if(length(slct_row) == 0) {
      R <- cbind(t(sigma_chol[slct_row, ]), Q[, seq_len(iter - 1), drop = FALSE])
    } else {
      R <- cbind(sigma_chol[slct_row, ], Q[, seq_len(iter - 1), drop = FALSE])
    }
    qr_object <- qr(R)
    qr_rank <- qr_object[["rank"]]
    set <- if(qr_rank == 0) {seq_len(M)} else {-seq_len(qr_rank)}
    N_i <- qr.Q(qr_object, complete = TRUE)[, set, drop = FALSE]
    N_stdn <- crossprod(N_i, rnorm(M, 0, 1))
    q_i <- N_i %*% (N_stdn / norm(N_stdn, type = "2"))
  }
  return(q_i)
}

#' @noRd
check_qi <- function(q_i, sigma_chol, sign_restr, M, iter, zero = FALSE, pos_check) {
  if(iter == 1 || zero) {
    v_sign_restr <- sign_restr[, iter]

    restricted <- which(!is.na(v_sign_restr) & v_sign_restr != 0)
    shock_vec <- sigma_chol %*% q_i
    shock_vec[abs(shock_vec) < 1e-12] <- 0
    shock_vec[which(shock_vec < 0)] <- -1
    shock_vec[which(shock_vec > 0)] <- 1

    if(identical(shock_vec[restricted], v_sign_restr[restricted])) {
      return(c(1L, iter))
    } else if (identical(-shock_vec[restricted], v_sign_restr[restricted])) {
      return(c(-1L, iter))
    } else {
      return(FALSE)
    }
  } else {
    for(j in which(pos_check == 0)) {
      v_sign_restr <- sign_restr[, j]

      restricted <- which(!is.na(v_sign_restr) & v_sign_restr != 0)
      shock_vec <- sigma_chol %*% q_i
      shock_vec[abs(shock_vec) < 1e-12] <- 0
      shock_vec[which(shock_vec < 0)] <- -1
      shock_vec[which(shock_vec > 0)] <- 1

      if(identical(shock_vec[restricted], v_sign_restr[restricted])) {
        return(c(1L, j))
      } else if (identical(-shock_vec[restricted], v_sign_restr[restricted])) {
        return(c(-1L, j))
      } else {
        return(FALSE)
      }
    }
  }
}
