coef.bvar <- function(x, conf_bands = 0.5, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  quantiles <- quantile_check(conf_bands)
  coefs <- apply(x[["beta"]], c(2, 3), quantile, quantiles)
  class(coefs) <- "bvar_coefs"

  return(coefs)
}

