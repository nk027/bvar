dummy <- function(
  mode = 1,
  min = 0.0001, max = 5,
  ...) {

  if(0 >= min || min >= max) stop("Boundaries misspecified.")
  if(mode < 0) stop("Parameter misspecified.")

  out <- list(mode = mode, min = min, max = max, ...)
  class(out) <- "bv_dummy"

  return(out)
}

bv_dummy <- function(
  mode = 1, sd = 1,
  min = 0.0001, max = 5,
  fun) {

  if(sd <= 0) stop("Parameter sd misspecified.")
  fun <- match.fun(fun)

  out <- dummy(mode, min, max, sd = sd, fun = fun,
               coef = gamma_coef(mode, sd))

  return(out)
}
