.onLoad <- function(...) {

  register_s3("coda", "as.mcmc", "bvar")
  register_s3("coda", "as.mcmc", "bvar_chains")

  invisible()
}

register_s3 <- function(pkg, generic, class) {

  fun <- get(paste0(generic, ".", class), envir = parent.frame())
  stopifnot(is.function(fun))

  if(pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}
