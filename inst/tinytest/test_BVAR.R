
# API tests -----

data <- data2 <- data3 <- matrix(rnorm(1000), nrow = 200)

# 10_bvar.R ---

expect_error(bvar(data, lags = 1, n_draw = 1000, n_burn = 1000))
expect_error(bvar(data, lags = 1, n_draw = 10, n_burn = 1))

expect_error(bvar(data = data[1:5, ], lags = 5))
expect_error(bvar(data = data, lags = 0))
data2[1:3, ] <- NA_real_
expect_error(bvar(data2, lags = 2))

expect_error(bvar(data, lags = 2, priors = NULL))
expect_error(bvar(data, lags = 2, mh = NULL))
expect_error(bvar(data, lags = 2, fcast = TRUE))
expect_error(bvar(data, lags = 2, irf = TRUE))

# 3*_metropolis ---

# 4*_priors ---

# 5*_fcast ---

# 6*_irf ---

# 80_coda.R ---

# 81_parallel.R ---

# 9*_methods ---

