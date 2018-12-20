.int_check <- function(x, min = 0, max = Inf,
                       msg = "There's a problem with an integer parameter.") {
  if(!is.numeric(x) || length(x) != 1 || x < min || x > max) {
    stop(msg)
  } else {
    return(as.integer(x))
  }
}
