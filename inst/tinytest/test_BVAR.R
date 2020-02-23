
# API tests -----

set.seed(42)

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

