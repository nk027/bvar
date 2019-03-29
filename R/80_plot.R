# trace

# density

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

