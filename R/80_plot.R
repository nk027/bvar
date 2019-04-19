plot.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  bv_plot(x, ...)

}


bv_plot <- function(x, mar = c(2, 2, 2, 0.5), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide an object of type bvar.")}

  y <- x[["hyper"]]
  K <- ncol(y)
  name <- colnames(y)
  bounds <- vapply(name, function(z) {
    c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])
  }, double(2))

  op <- par(mfrow = c(K + 1, 2), mar = mar, ...)

  plot_trace(x[["ml"]], name = "marginal likelihood")
  plot_dens(x[["ml"]], name = "marginal likelihood")
  for(i in 1:K) {
    plot_trace(y[, i], name[i], bounds[, i])
    plot_dens(y[, i], name[i], bounds[, i])
  }

  par(op)

  return(invisible(x))
}


plot_hyper <- function(x, name, fun = c(plot_trace, plot_dens), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(missing(name)) {
    stop("Please set `name` to specify a parameter to plot.")
  } else if(!name %in% c("ml", colnames(x[["hyper"]]))) {
    stop("Parameter named '", name, "' not found.")
  }

  dots <- list(...)
  lapply(dots, function(x) {
    if(!inherits(x, "bvar")) {stop("Provide `bvar` objects to the ellipsis.")}
  })

  if(name == "ml") {
    y <- x[["ml"]]
    dots <- lapply(dots, function(x) x[["ml"]])
  } else {
    y <- x[["hyper"]][, which(colnames(x[["hyper"]]) == name)]
    dots <- lapply(dots, function(x) {
      x[["hyper"]][, which(colnames(x[["hyper"]]) == name)]
    })
  }
  bounds <- vapply(name, function(z) {
    c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])}, double(2))

  fun(y, name, bounds, dots)

  return(invisible(x))
}


bv_plot_trace <- function(x, name, ...) {

  plot_hyper(x, name, fun = plot_trace, ...)

}

bv_plot_density <- function(x, name, ...) {

  plot_hyper(x, name, fun = plot_dens, ...)

}


plot_trace <- function(x, name = NULL, bounds = NULL, dots = list()) {

  ylim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))

  plot(x, type = "l", xlab = "Index", ylab = "Value", ylim = ylim,
       main = paste("Trace", if(!is.null(name)) {paste("of", name)} else {""}))
  for(dot in dots) {lines(dot, col = "lightgray")}
  abline(h = bounds, lty = "dashed", col = "darkgray")

  return(invisible(x))
}


plot_dens <- function(x, name = NULL, bounds = NULL, dots = list()) {

  xlim <- c(min(vapply(dots, min, double(1)), x),
            max(vapply(dots, max, double(1)), x))

  plot(density(x), xlim = xlim,
       main = paste("Trace", if(!is.null(name)) {paste("of", name)} else {""}))
  polygon(density(x), col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
  for(dot in dots) {
    polygon(density(dot), col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
  }
  lines(density(x))
  abline(v = bounds, lty = "dashed", col = "darkgray")

  return(invisible(x))
}
