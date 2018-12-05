bvar <- function(data, lags,
  draws = 10000, burns = 10000, thin = 1,
  mh,
  priors,
  fcast,
  irf,
  sv,
  verbose = FALSE, ...) {

  # Input Checking ----------------------------------------------------------

  # Data
  if(!is.numeric(data) || any(is.na(data)) || length(data) < 2) {
    stop("Problem with the data. Make sure it's numeric without NAs.")
  } else {
    Y <- as.matrix(data)
  }

  # Integers
  lags <- .int_check(lags, min = 1, max = nrow(Y))
  draws <- .int_check(draws, min = 1)
  burns <- .int_check(burns, min = 0)
  thin <- .int_check(thin, min = 1, max = draws / 10)

  # Constructors
  if(!"bvar_prior" %in% class(priors)) stop()
  if(!"bvar_fcast" %in% class(fcast)) stop()
  if(!"bvar_irf" %in% class(irf)) stop()
  if(!"bvar_sv" %in% class(sv)) stop()


  # Preparation -------------------------------------------------------------

  X <- var_lag(Y, lags)

  Y <- Y[(lags + 1):nrow(Y), ]
  X <- X[(lags + 1):nrow(X), ]
  X <- cbind(1, X)

  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)



}
