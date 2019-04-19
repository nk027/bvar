plot.bvar_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  mar = c(2, 2, 2, 0.5)) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_fcast` object.")}
  bv_plot_fcast(x, conf_bands, variables, vars)

}


bv_plot_fcast <- function(
  x,
  conf_bands = 0.16,
  variables = NULL,
  vars = NULL,
  mar = c(2, 2, 2, 0.5)) {

  quantiles <- quantile_check(conf_bands)

  if(inherits(x, "bvar")) {
    y <- apply(x[["fcast"]][["fcast"]], c(2, 3), quantile, quantiles)
    M <- dim(y)[3]
    P <- dim(y)[1]
    if(is.null(variables)) {
      variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
    }

  } else if(inherits(x, "bvar_fcast")) {
    y <- apply(x[["fcast"]], c(2, 3), quantile, quantiles)
    M <- dim(y)[3]
    P <- dim(y)[1]
    if(is.null(variables)) {variables <- 1:M}

  } else {stop("Please provide a `bvar` or `bvar_fcast` object.")}

  if(length(variables) != M) {stop("Named vector `variables` is incomplete.")}

  col <- set_gray(P)
  pos <- get_var_pos(vars, variables, M)

  plot_irf(y, variables, pos, col, mar)

  return(invisible(x))
}


plot_fcast <- function(
  x,
  variables,
  pos,
  col, mar) {

  op <- par(mfrow = c(length(pos), 1), mar = mar)
  for(i in pos) {
    ts.plot(t(as.matrix(x[, , i])),
            col = col, lty = 1,
            main = paste("Forecast", variables[i]))
    grid()
  }
  par(op)
}
