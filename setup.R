to_source <- c(10, 11, 12, 20, 30, 21, 40, 41, 42, 50, 60:65, 90)

sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})

data <- matrix(rnorm(200), ncol = 2)
lags <- 2
n_draw <- 10000
n_burn <- 5000
thin <- 1
fcast <- bv_fcast()
irf <- bv_irf()
verbose <- TRUE
priors <- bv_priors()
metropolis <- bv_metropolis()

x <- bvar(data, lags, n_draw, n_burn, thin,
          priors, metropolis, fcast, irf, verbose = TRUE)
y <- bvar(data, lags, n_draw, n_burn, thin,
          priors = bv_priors(hyper = c("lambda", "alpha")),
          metropolis, fcast, irf, verbose = TRUE)
z <- bvar(data, lags, n_draw, n_burn, thin,
          priors, metropolis, irf = irf, verbose = TRUE)
w <- bvar(data, lags, n_draw, n_burn, thin = 3, irf = irf, verbose = TRUE)
v <- bvar(data, lags, n_draw, n_burn, thin = 3, verbose = TRUE)
