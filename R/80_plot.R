hyper_plot <- function(x) {

  if(!inherits(x, "bvar")) stop()

  K <- ncol(x[["hyper"]])
  name <- x[["priors"]][["hyper"]]

  op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(K + 1, 2))

  trace_plot(x[["ml"]], "marginal likelihood")
  dens_plot(x[["ml"]], "marginal likelihood")
  for(i in 1:K) {
    trace_plot(x[["hyper"]][, i], name[i], x[["priors"]][[name[i]]][["min"]],
               x[["priors"]][[name[i]]][["max"]])
    dens_plot(x[["hyper"]][, i], name[i], x[["priors"]][[name[i]]][["min"]],
              x[["priors"]][[name[i]]][["max"]])
  }
  par(op)

  invisible(x)
}

# trace

trace_plot <- function(x, name, min = NULL, max = NULL, ...) {

  dots <- list(...)
  ylim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))
  plot(x, type = "l", xlab = "Index", ylab = "Value", ylim = ylim,
       main = paste("Trace of", name))
  for(dot in dots) lines(dot, col = "lightgray")
  # abline(h = mean(x), lty = "dotted", col = "gray") # Mean
  abline(h = c(min, max), lty = "dashed", col = "darkgray")

  invisible(x)
}

# density

dens_plot <- function(x, name, min = NULL, max = NULL, ...) {

  dots <- list(...)
  xlim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))
  plot(density(x), main = paste("Density of", name), xlim = xlim)
  for(dot in dots) lines(density(dot), col = "lightgray")
  # abline(v = x[length(x)], col = "gray") # Last position
  abline(v = c(min, max), lty = "dashed", col = "darkgray")

  invisible(x)
}

hist_plot <- function(x, name, min = NULL, max = NULL) {

  hist(x, xlab = "Value", main = paste("Histogram of", name))
  # abline(v = x[length(x)], col = "gray") # Last position
  abline(v = c(min, max), lty = "dashed", col = "darkrgray")
  invisible(x)

}

# irf

irf_plot <- function(bvar_obj, sign_level = c(0.05, 0.16), var_names = NULL) {

  if(!inherits(bvar_obj, "bvar")) stop("Please provide object of type bvar.")

  if(any(!is.numeric(sign_level),
         any(sign_level > 1), any(sign_level < 0),
         length(sign_level) > 2)){
    stop("Level(s) of significance misspecified.")
  }

  irf_store <- bvar_obj[["irf"]][["irf_store"]]
  M <- bvar_obj[["meta"]][["M"]]

  quants <- sort(c(sign_level, 0.5, (1 - sign_level)))
  irf_quants <- apply(irf_store, c(2, 3, 4), quantile, quants, na.rm = TRUE)

  main <- if(!is.null(var_names)) {paste0(var_names, " impulse")} else {NULL}
  ngray <- length(sign_level)
  col <- c(rep("darkgray", ngray), "black", rep("darkgray", ngray))

  op <- par(no.readonly = TRUE)
  par(mfrow = c(M, M), mar = c(2, 2, 2, 1))
  for(i in 1:M) {
    for(j in 1:M) {
      ts.plot(t(as.matrix(irf_quants[, i, , j])),
              col = col, lty = 1,
              main = main[[j]],
              ylab = "", xlab = "")
      abline(h = 0, col = "black")
    }
  }
  par(op)

  return(invisible(irf_quants))
}
# fevd

# fcast

fcast_plot <- function(x, quantiles = c(0.16, 0.5, 0.84)) {

  x <- apply(x, c(2, 3), quantile, quantiles)

  M <- dim(x)[3]
  P <- dim(x)[1]

  n_gray <- if(P %% 2 == 0) 0 else P %/% 2
  col <- c(rep("darkgray", n_gray), "black", rep("darkgray", n_gray))

  op <- par(mfrow = c(M, 1), mar = c(2, 2, 2, 0.5))

  for(i in 1:M) {
    ts.plot(t(as.matrix(x[, , i])),
            col = col, lty = 1,
            main = "<Variable>")
    grid()
  }
  par(op)

  invisible(x)
}
