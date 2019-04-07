bv_fcast <- function(
  horizon = 12,
  conditional = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(!is.null(conditional)) stop("Conditional forecasts not yet implemented.")

  out <- list("horizon" = horizon, "conditional" = conditional)
  class(out) <- "bv_fcast"

  return(out)
}
