plot.bvar_irf <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars_impulse = NULL,
  vars_response = NULL,
  mar = c(2, 2, 2, 0.5)) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}
  bv_plot_irf(x, conf_bands, variables, vars_impulse, vars_response)

}


bv_plot_irf <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars_impulse = NULL,
  vars_response = NULL,
  mar = c(2, 2, 2, 0.5)) {

  quantiles <- quantile_check(conf_bands)

  if(inherits(x, "bvar")) {
    y <- apply(x[["irf"]][["irf"]], c(2, 3, 4), quantile, quantiles)
    M <- dim(y)[2]
    P <- dim(y)[1]
    if(is.null(variables)) {
      variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
    }

  } else if(inherits(x, "bvar_irf")) {
    y <- apply(x[["irf"]], c(2, 3, 4), quantile, quantiles)
    M <- dim(y)[2]
    P <- dim(y)[1]
    if(is.null(variables)) {variables <- 1:M}

  } else {stop("Please provide an object of type bvar or bvar_irf.")}

  if(length(variables) != M) {stop("Named vector `variables` is incomplete.")}

  col <- set_gray(P)
  pos_imp <- get_var_set(vars_impulse, variables, M)
  pos_res <- get_var_set(vars_response, variables, M)

  plot_irf(y, variables, pos_imp, pos_res, col, mar)

  return(invisible(x))
}


plot_irf <- function(
  x,
  variables,
  pos_imp,
  pos_res,
  col, mar) {

  op <- par(mfrow = c(length(pos_res), length(pos_imp)), mar = mar)
  for(i in pos_res) {
    for(j in pos_imp) {
      ts.plot(t(as.matrix(x[, i, , j])),
              col = col, lty = 1,
              main = paste("Shock", variables[j], "on", variables[i]))
      abline(h = 0, lty = "dashed", col = "black")
      grid()
    }
  }
  par(op)
}
