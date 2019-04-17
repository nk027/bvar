bv_plot_irf <- function(
  x,
  vars_impulse,
  vars_response,
  conf_bands = 0.16,
  mar = c(2, 2, 2, 0.5), col,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object of type bvar.")}

  quantiles <- quantile_check(conf_bands)

  y <- apply(x[["irf"]][["irf"]], c(2, 3, 4), quantile, quantiles)
  M <- dim(y)[2]
  P <- dim(y)[1]

  variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
  if(missing(col)) {col <- set_gray(P)}
  pos_imp <- choose_vars(vars_impulse, x[["variables"]], M)
  pos_res <- choose_vars(vars_response, x[["variables"]], M)

  op <- par(mfrow = c(length(pos_res), length(pos_imp)), mar = mar, ...)
  for(i in pos_res) {
    for(j in pos_imp) {
      ts.plot(t(as.matrix(y[, i, , j])),
              col = col, lty = 1,
              main = paste("Shock", variables[j], "on", variables[i]))
      abline(h = 0, lty = "dashed", col = "black")
    }
  }
  par(op)

  return(invisible(x))
}


bv_plot_fcast <- function(
  x,
  vars,
  conf_bands = 0.16,
  mar = c(2, 2, 2, 0.5), col,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object of type bvar.")}

  quantiles <- quantile_check(conf_bands)

  y <- apply(x[["fcast"]][["fcast"]], c(2, 3), quantile, quantiles)
  M <- dim(y)[3]
  P <- dim(y)[1]

  variables <- if(is.null(x[["variables"]])) {1:M} else {x[["variables"]]}
  if(missing(col)) {col <- set_gray(P)}
  pos <- choose_vars(vars, x[["variables"]], M)

  op <- par(mfrow = c(length(pos), 1), mar = mar, ...)
  for(i in pos) {
    ts.plot(t(as.matrix(y[, , i])),
            col = col, lty = 1,
            main = paste("Forecast", variables[i]))
    grid()
  }
  par(op)

  return(invisible(x))
}
