bv_minnesota <- function(
  lambda = bv_lambda(0.2, 0.4, 0.0001, 5), # mode, sd, min, max
  alpha = bv_alpha(2, 0.1, 0.5), # mode, min, max
  psi = "auto", var = 1e07) {

  if(!is(lambda, "bv_dummy") && !is(alpha, "bv_dummy")) {
    stop("Please use 'bv_lambda' and/or 'bv_alpha' to set lambda and alpha.")
  }

  out <- list(lambda = lambda, alpha = alpha, psi = psi, var = var)
  class(out) <- "bv_minnesota"

  return(out)
}


bv_lambda <- function(mode = 0.2, sd = 0.4, min = 0.0001, max = 50) {

  if(sd <= 0) stop("Parameter sd misspecified.")

  out <- dummy(mode, min, max, sd = sd)

  return(out)
}


bv_alpha <- function(mode = 0.2, min = 0.1, max = 0.5) {

  out <- dummy(mode, min, max)

  return(out)
}
