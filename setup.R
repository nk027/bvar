to_source <- c(11, 12, 15, 20, 40)

sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})

data <- matrix(rnorm(200), ncol = 2)
lags <- 2
draws <- 10000
burns <- 10000
thin <- 1
verbose <- TRUE
priors <- bv_priors()
# priors <- bv_priors(soc = bv_dummy(fun = mean))
metropolis <- bv_metropolis()

bvar()
