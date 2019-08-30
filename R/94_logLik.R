# Issues:
#   - not vectorised
#   - additional dependency
#     - maybe use mvtnorm instead of MASS
#     - go GPL on their ass and use the functions directly
#     - Write own vectorised version
#   - no checks
#' @noRd
logLik.bvar <- function(x, ...) {

  Y <- x[["meta"]][["Y"]]
  X <- x[["meta"]][["X"]]
  N <- x[["meta"]][["N"]]
  beta <- coef(x, 0.5)
  sigma <- vcov(x, 0.5)

  ll <- 0
  for(i in seq_len(N)) {
    ll <- ll + mvtnorm::dmvnorm(Y[i, ], X[i, ] %*% beta, sigma, log = TRUE)
  }

}

isSymmetric.bvar_vcovs <- function(object, ...) {isSymmetric.matrix(object, ...)}
