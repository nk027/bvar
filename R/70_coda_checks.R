#' @noRd
as.mcmc.bvar <- function(x, vars = NULL, chains = list(), ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(z) {if(!inherits(z, "bvar")) {
    stop("Please provide `bvar` objects to the chains.")
  }})

  has_coda()

  y <- cbind(ml = x[["ml"]], x[["hyper"]])

  if(is.null(vars)) {
    vars <- c("ml", colnames(y))
  }

  if(length(chains) != 0){
    chains <- lapply(chains, function(z) {cbind(ml = z[["ml"]], z[["hyper"]])})
    apply(sapply(chains, colnames), 2, function(z) if(!all(vars %in% z)) {
      stop("Parameter(s) named '",
           paste0(vars[which(!vars %in% z)], collapse = ", "),
           "' not found in all provided chains.")
    })
    chains[[deparse(substitute(x))]] <- y
    chains <- lapply(chains, function(z) {z[ , which(colnames(z) %in% vars)]})
    out <- as.mcmc.list(lapply(chains, as.mcmc))
  } else {
    if(!all(vars %in% c("ml", colnames(y)))) {
      stop("Parameter(s) named '",
           paste0(vars[which(!vars %in% c("ml", colnames(y)))], collapse = ", "),
           "' not found.")
    }
    out <- as.mcmc(y[ , which(colnames(y) %in% vars)], ...)
  }

  return(out)
}


#' @noRd
has_coda <- function() {
  if(!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \'coda\' required for this method.", call. = FALSE)
  }
}
