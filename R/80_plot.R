plot.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  bv_plot(x, ...)

}
