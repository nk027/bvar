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
  if(hyper[[1]] == "auto") {
    hyper <- c(if(!is.null(mn)) "lambda", names(dots))
  } else {
    full <- c(if(!is.null(mn)) c("lambda", "alpha", "psi"), names(dots))
    if(hyper[[1]] == "full") {
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
