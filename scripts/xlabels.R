
#' @noRd
xax_at <- function(N, t_back = 0) {
  out <- list()
  if(N <= 8) {
    out[["at"]] <- seq(N)
    out[["labs"]] <- seq(N) - t_back
  } else if(N <= 16) {
    out[["at"]] <- seq(2, N, by = 2)
    out[["labs"]] <- out[["at"]] - t_back
  } else if(N <= 32) {
    out[["at"]] <- seq(4, N, by = 4)
    out[["labs"]] <- out[["at"]] - t_back
  } else {
    steps <- 2 * (N %/% 10)
    out[["at"]] <- seq(0, N, by = steps)
    out[["labs"]] <- seq(1, N + 1, by = steps) - t_back
  }
  cat("at: ", paste0(out[["at"]], collapse = ", "), sep = "",
    "\nlabs: ", paste0(out[["labs"]], collapse = ", "))
  axis(1, at = out[["at"]], labels = out[["labs"]])
  # return(out)
}
