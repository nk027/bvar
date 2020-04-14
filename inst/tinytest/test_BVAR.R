
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

expect_silent(bv_metropolis(scale_hess = c(0.1, 0.05), adjust_acc = TRUE,
  adjust_burn = 0.5, acc_lower = 0.1, acc_upper = 0.9, acc_change = 0.1))
expect_silent(mh <- bv_mh(scale_hess = 0.1, adjust_acc = TRUE,
  adjust_burn = 0.5, acc_lower = 0.1, acc_upper = 0.9, acc_change = 0.1))

expect_error(bv_mh(scale_hess = -1))
expect_error(bv_mh(adjust_acc = TRUE, adjust_burn = 0))
expect_error(bv_mh(adjust_acc = TRUE, acc_lower = 0.5, acc_upper = 0.4))
expect_error(bv_mh(adjust_acc = TRUE, acc_change = -1))


# 4*_priors.R ---

expect_silent(bv_minnesota(lambda = bv_lambda(0.25, sd = 0.4),
  alpha = bv_alpha(mode = 1.5, min = 0.5, max = 5), var = 1e06))
expect_silent(mn <- bv_mn(lambda = bv_lambda(0.2, sd = 0.4, max = 4.5),
  alpha = bv_alpha(mode = 1.5, min = 0.5, max = 5), var = 1e08))

expect_silent(dummy <- bv_dummy(fun = function(Y, lags, par) {
  return(list(Y = Y[1, ] * par, X = c(1, rep(Y[1, ] * par, lags))))}))
expect_silent(bv_soc(mode = 1.5, sd = 2, min = 1e-7, max = 100))
expect_silent(bv_sur(mode = 2, sd = 1, min = 0.01, max = 50))

expect_silent(priors <- bv_priors(hyper = "auto", mn = mn,
  sur = bv_sur(), soc = bv_soc(), custom = dummy))
expect_equal(
  bv_priors(hyper = c("lambda", "alpha", "psi")),
  bv_priors(hyper = c("full")))

expect_silent(bv_mn(lambda = c(0.2, 0.4, 1e-6, 5),
  alpha = c(1.5, 0.5, 0.1, 5), var = 100))
expect_silent(bv_mn(psi = bv_psi(scale = 0.2, shape = 0.2,
  mode = c(1, 1.5, 1.2, 0.4), min = rep(0.001, 4))))
expect_silent(bv_mn(psi = bv_psi(scale = 0.2, shape = 0.2,
  mode = c(1, 1.5, 1.2, 0.4), max = rep(1000, 4))))

# Faulty sd, dummy prior and hyperparameters
expect_error(bv_priors(mn = bv_lambda(sd = 0)))
expect_error(bv_priors(mn = bv_mn(), dummy = list("mode" = 1, "sd" = 1)))
expect_error(bv_priors(hyper = c("lambda", "alpha", "soc"), sur = bv_sur()))

# Wrong format for alpha, faulty sd and var
expect_error(bv_mn(alpha = c(2, 1)))
expect_error(bv_mn(lambda = bv_lambda(mode = 0.4, sd = 0)))
expect_error(bv_mn(var = -1))

# Boundaries w/o mode, faulty mode, wrong length, wrong boundaries
expect_error(bv_mn(bv_psi(min = c(0, 0), max = c(1, 1))))
expect_error(bv_mn(bv_psi(mode = c(1, 2, 0))))
expect_error(bv_mn(bv_psi(mode = c(1, 2, 1), min = c(0.1, 0.1))))
expect_error(bv_mn(bv_psi(mode = c(1, 2), min = c(0.1, 0.5), max = c(1, 0.1))))

# Faulty sd, wrong boundaries
expect_error(bv_dummy(mode = 2, sd = 0))
expect_error(bv_dummy(min = 2, max = 1))


# 5*_fcast ---

expect_silent(bv_fcast(horizon = 2020))
expect_silent(bv_fcast(cond_path = c(2, 2, 2, 2), cond_vars = 1))
expect_silent(bv_fcast(cond_path = c(2, 2, 2, 2), cond_vars = "FEDFUNDS"))
expect_silent(bv_fcast(cond_path = matrix(rep(2, 6), nrow = 3)))
expect_silent(bv_fcast(
  cond_path = matrix(c(2, 2, NA, 1.5, NA, NA, 1, 1.2, 1.5), nrow = 3)))
expect_message(bv_fcast(horizon = 4, cond_path = rep(2, 6), cond_vars = 1))
expect_error(bv_fcast(cond_path = matrix(rnorm(9), nrow = 3),
  cond_vars = c(1, 1)))
expect_error(bv_fcast(cond_path = matrix(rnorm(9), nrow = 3),
  cond_vars = c("FEDFUNDS", "FEDFUNDS", "GDP")))


# 6*_irf ---

# 80_coda.R ---

# 81_parallel.R ---

# 9*_methods ---

