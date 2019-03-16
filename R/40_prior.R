bv_priors <- function(
  hyper = "auto",
  mn = bv_minnesota(bv_lambda(0.2, 0.4, 0.0001, 5),
                    bv_alpha(2, 0.1, 0.5),
                    psi = "auto", var = 1e07),
  ...) {

  # Check inputs ------------------------------------------------------------

  if(!is.null(mn) && !is(mn, "bv_minnesota")) {
    stop("Please use 'bv_minnesota' to set the minnesota prior.")
  }
  dots <- list(...)
  if(!all(vapply(dots, is, TRUE, "bv_dummy"))) {
    stop("Please use 'bv_dummy' to set dummy priors.")
  }
  if(hyper == "auto") {
    hyper <- c(if(!is.null(mn)) "lambda", names(dots))
  } else {
    full <- c(if(!is.null(mn)) c("lambda", "alpha", "psi"), names(dots))
    if(hyper == "full") {
      hyper <- full
    } else {
      if(!all(hyper %in% full)) stop("Hyperprior not found.")
    }
  }


  # Output ------------------------------------------------------------------

  out <- if(!is.null(mn)) {
    list(hyper = hyper, lambda = mn[["lambda"]], alpha = mn[["alpha"]],
         psi = mn[["psi"]], var = mn[["var"]], ...)
  } else {
    list(hyper = hyper, ...)
  }
  class(out) <- "bv_priors"

  return(out)
}

.bv_dummy <- function(
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

  out <- .bv_dummy(mode, min, max,
                   sd = sd, fun = fun,
                   coef = gamma_coef(mode, sd))

  return(out)
}

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

  out <- .bv_dummy(mode, min, max, sd = sd)

  return(out)
}

bv_alpha <- function(mode = 0.2, min = 0.1, max = 0.5) {

  out <- .bv_dummy(mode, min, max)

  return(out)
}
