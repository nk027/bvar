hyper_plot <- function(x) {

  if(!inherits(x, "bvar")) stop("Please provide an object of type bvar.")

  y <- x[["hyper"]]
  K <- ncol(y)
  name <- colnames(y)
  bounds <- vapply(name, function(z) {
    c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])}, double(2))

  op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(K + 1, 2))

  trace_plot(x[["ml"]], "marginal likelihood")
  dens_plot(x[["ml"]], "marginal likelihood")
  for(i in 1:K) {
    trace_plot(y[, i], name[i], bounds[, i])
    dens_plot(y[, i], name[i], bounds[, i])
  }
  par(op)

  invisible(x)
}

# trace

trace_plot <- function(x, name, bounds = NULL, ...) {

  dots <- list(...)
  ylim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))
  plot(x, type = "l", xlab = "Index", ylab = "Value", ylim = ylim,
       main = paste("Trace of", name))
  for(dot in dots) lines(dot, col = "lightgray")
  # abline(h = mean(x), lty = "dotted", col = "gray") # Mean
  abline(h = bounds, lty = "dashed", col = "darkgray")

  invisible(x)
}

# density

dens_plot <- function(x, name, bounds = NULL, ...) {

  dots <- list(...)
  xlim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))
  plot(density(x), main = paste("Density of", name), xlim = xlim)
  polygon(density(x), col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
  for(dot in dots)
    polygon(density(dot), col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
  lines(density(x))
  # abline(v = x[length(x)], col = "gray") # Last position
  abline(v = bounds, lty = "dashed", col = "darkgray")

  invisible(x)
}

hist_plot <- function(x, name, bounds = NULL) {

  hist(x, xlab = "Value", main = paste("Histogram of", name))
  # abline(v = x[length(x)], col = "gray") # Last position
  abline(v = bounds, lty = "dashed", col = "darkgray")

  invisible(x)
}

# irf

irf_plot <- function(
  x,
  conf_bands = 0.16,
  variables,
  mar = c(2, 2, 2, 0.5), col,
  ...) {

  if(!inherits(x, "bvar")) stop("Please provide an object of type bvar.")

  quantiles <- sort(c(conf_bands, 0.5, (1 - conf_bands)))
  if(any(!is.numeric(quantiles), any(quantiles > 1), any(quantiles < 0))) {
    stop("Quantiles misspecified.")
  }

  y <- apply(x[["irf"]][["irf"]], c(2, 3, 4), quantile, quantiles, na.rm = TRUE)

  M <- dim(y)[2]
  P <- dim(y)[1]

  if(missing(variables)){
    variables <- x[["variables"]]
  } else if(length(variables) != M){
    stop("Number of names for variables provided does not match number of variables.")
  }

  if(missing(col)) {
    n_gray <- if(P %% 2 == 0) 0 else P %/% 2
    col <- c(rep("darkgray", n_gray), "black", rep("darkgray", n_gray))
  }

  op <- par(mfrow = c(M, M), mar = mar, ...)
  for(i in 1:M) {
    for(j in 1:M) {
      ts.plot(t(as.matrix(y[, i, , j])),
              col = col, lty = 1,
              main = variables[i])
      abline(h = 0, lty = "dashed", col = "black")
    }
  }
  par(op)

  invisible(x)
}

# fcast

fcast_plot <- function(
  x,
  conf_bands = 0.16,
  variables,
  mar = c(2, 2, 2, 0.5), col,
  ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object of type bvar.")}

  quantiles <- sort(c(conf_bands, 0.5, (1 - conf_bands)))
  if(any(!is.numeric(quantiles), any(quantiles > 1), any(quantiles < 0))) {
    stop("Quantiles misspecified.")
  }

  y <- apply(x[["fcast"]], c(2, 3), quantile, quantiles, na.rm = TRUE)

  M <- dim(y)[3]
  P <- dim(y)[1]

  if(missing(variables)){
    variables <- x[["variables"]]
  } else if(length(variables) != M){
    stop("Number of names for variables provided does not match number of variables.")
  }


  if(missing(col)) {
    n_gray <- if(P %% 2 == 0) 0 else P %/% 2
    col <- c(rep("darkgray", n_gray), "black", rep("darkgray", n_gray))
  }

  op <- par(mfrow = c(M, 1), mar = mar, ...)
  for(i in 1:M) {
    ts.plot(t(as.matrix(y[, , i])),
            col = col, lty = 1,
            main = variables[i])
    grid()
  }
  par(op)

  invisible(x)
}
