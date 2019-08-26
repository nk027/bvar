library("MASS")

to_source <- c(0:99)
sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})


# Simulated data ----------------------------------------------------------

data <- matrix(rnorm(20000), ncol = 5)
lags <- 5
n_draw <- 10000
n_burn <- 0
n_thin <- 1
fcast <- bv_fcast()
irf <- bv_irf()
verbose <- TRUE
priors <- bv_priors()
mh <- bv_mh()

run1 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors, mh, fcast, irf, verbose = TRUE)
run2 <- bvar(data, lags, n_draw, n_burn, n_thin,
             fcast = NULL, verbose = TRUE)

predict(run1)
predict(run2)
predict(run2, horizon = 14L)
predict(run1, bv_fcast(8L))
