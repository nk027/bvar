#' @noRd
as.mcmc.bvar <- function(x, vars = NULL, ...) {

  if(!inherits(x, "bvar_irf")) {stop("Please provide a `bvar_irf` object.")}

  has_coda()


  if(is.null(vars)) {
    vars <- c("ml", colnames(x[["hyper"]]))
  } else if {
    if(!all(vars %in% c("ml", colnames(x[["hyper"]])))) {
      stop("Parameter named '", vars[which(!vars %in% c("ml", colnames(y)))],
           "' not found.")
    }
  }

  out <- coda::as.mcmc(y, ...)
  return(out)
}


#' @noRd
has_coda <- function() {
  if(!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \'coda\' required for this method.", call. = FALSE)
  }
}
