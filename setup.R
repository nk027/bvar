to_source <- c(0:99)

sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})

data <- matrix(rnorm(200), ncol = 2)
lags <- 2
n_draw <- 10000
n_burn <- 5000
n_thin <- 1
fcast <- bv_fcast()
irf <- bv_irf()
verbose <- TRUE
priors <- bv_priors()
metropolis <- bv_metropolis()

run1 <- bvar(data, lags, n_draw, n_burn, thin,
          priors, metropolis, fcast, irf, verbose = TRUE)
run2 <- bvar(data, lags, n_draw, n_burn, thin,
          priors = bv_priors(hyper = c("lambda", "alpha")),
          metropolis, fcast, irf, verbose = TRUE)
run3 <- bvar(data, lags, n_draw, n_burn, thin,
          priors, metropolis, fcast, irf, verbose = TRUE)
run4 <- bvar(data, lags, n_draw, n_burn, thin = 3, irf = NULL, verbose = TRUE)
run5 <- bvar(data, lags, n_draw, n_burn, thin = 3, fcast = NULL, verbose = TRUE)
run6 <- bvar(data, lags, n_draw, n_burn, thin,
             priors = bv_priors(hyper = c("lambda", "alpha", "psi")),
             metropolis, fcast, irf, verbose = TRUE)
