
# API tests -------

data <- data2 <- data3 <- matrix(rnorm(1000), nrow = 200)

# Fail and prepare -----

# 10_bvar ---

# n_ shenaningans
expect_error(bvar(data, lags = 1, n_draw = 1000, n_burn = 1000))
expect_error(bvar(data, lags = 1, n_draw = 10, n_burn = 1))

# Erroneous data and lags
expect_error(bvar(data = data[1:5, ], lags = 5))
expect_error(bvar(data = data, lags = 0))
data2[1:3, ] <- NA_real_
expect_error(bvar(data2, lags = 2))

# Faulty arguments
expect_error(bvar(data, lags = 2, priors = NULL))
expect_error(bvar(data, lags = 2, mh = NULL))
expect_error(bvar(data, lags = 2, fcast = TRUE))
expect_error(bvar(data, lags = 2, irf = TRUE))

# 3*_metropolis ---

# Proper use
expect_silent(bv_metropolis(scale_hess = c(0.1, 0.05), adjust_acc = TRUE,
  adjust_burn = 0.5, acc_lower = 0.1, acc_upper = 0.9, acc_change = 0.1))
expect_silent(mh <- bv_mh(scale_hess = 0.1, adjust_acc = TRUE,
  adjust_burn = 0.5, acc_lower = 0.1, acc_upper = 0.9, acc_change = 0.1))
expect_silent(print(mh))

# Faulty arguments
expect_error(bv_mh(scale_hess = -1))
expect_error(bv_mh(adjust_acc = TRUE, adjust_burn = 0))
expect_error(bv_mh(adjust_acc = TRUE, acc_lower = 0.5, acc_upper = 0.4))
expect_error(bv_mh(adjust_acc = TRUE, acc_change = -1))


# 4*_priors ---

# Proper use
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
expect_silent(print(priors))
expect_equal(
  bv_priors(hyper = c("lambda", "alpha", "psi")),
  bv_priors(hyper = c("full")))

expect_silent(print(bv_mn(lambda = c(0.2, 0.4, 1e-6, 5),
  alpha = c(1.5, 0.5, 0.1, 5), var = 100)))
expect_silent(print(bv_mn(psi = bv_psi(scale = 0.2, shape = 0.2,
  mode = c(1, 1.5, 1.2, 0.4), min = rep(0.001, 4)))))
expect_silent(print(bv_mn(psi = bv_psi(scale = 0.2, shape = 0.2,
  mode = c(1, 1.5, 1.2, 0.4), max = rep(1000, 4)))))

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

# Proper use
expect_silent(opt_fcast1 <- bv_fcast())
expect_silent(print(opt_fcast1))
expect_silent(opt_fcast2 <- bv_fcast(cond_path = c(2, 2, 2, 2), cond_vars = 1))
expect_silent(bv_fcast(cond_path = c(2, 2, 2, 2), cond_vars = "FEDFUNDS"))
expect_silent(bv_fcast(cond_path = matrix(rep(2, 6), nrow = 3)))
expect_silent(bv_fcast(horizon = 2020,
  cond_path = matrix(c(2, 2, NA, 1.5, NA, NA, 1, 1.2, 1.5), nrow = 3)))

# Short horizon and duplicated cond_vars
expect_message(bv_fcast(horizon = 4, cond_path = rep(2, 6), cond_vars = 1))
expect_error(bv_fcast(cond_path = matrix(rnorm(9), nrow = 3),
  cond_vars = c(1, 1)))
expect_error(bv_fcast(cond_path = matrix(rnorm(9), nrow = 3),
  cond_vars = c("FEDFUNDS", "FEDFUNDS", "GDP")))


# 6*_irf ---

# Proper use
expect_silent(opt_irf1 <- bv_irf(fevd = TRUE))
expect_silent(print(opt_irf1))
expect_silent(bv_irf(horizon = 2020, identification = FALSE))
expect_silent(opt_irf2 <- bv_irf(fevd = FALSE, # Sign restricted
  sign_restr = matrix(c(1, NA, NA, NA, 1, -1, -1, 1, NA), nrow = 3)))
expect_silent(print(opt_irf2))
expect_silent(opt_irf3 <- bv_irf(fevd = FALSE, # Zero sign restricted
  sign_restr = matrix(c(NA, 0, NA, NA, 1, -1, NA, 1, NA), nrow = 3)))
expect_silent(bv_irf(sign_restr = c(1, NA, -1, 1), sign_lim = 1000))
expect_silent(bv_irf(sign_restr = c(0, NA, NA, 1), sign_lim = 1000))

# Underidentified, too many 0, non-square restrictions
expect_message(bv_irf(sign_restr = matrix(c(NA, NA, NA, NA), nrow = 2)))
expect_error(bv_irf(sign_restr = matrix(c(0, 0, -1, NA), nrow = 2)))
expect_error(bf_irf(sign_restr = matrix(rnorm(6), nrow = 3)))


# Run and analyse -----

# 10_bvar ---

# Base run
expect_silent(run <- bvar(data, lags = 2, priors = priors, mh = mh))
# Conditional and sign-restricted
expect_silent(run2 <- bvar(data[, 1:3], lags = 2,
  fcast = opt_fcast2, irf = opt_irf2))


# 5*_fcast ---

# Ex-post predicts and methods
expect_silent(predict(run) <- predict(run, opt_fcast1))
expect_silent(fcasts1 <- predict(run))
expect_silent(fcasts2 <- predict(run2))

expect_silent(print(fcasts1))
expect_silent(print(summary(fcasts1)))
expect_silent(print(summary(fcasts2)))
expect_silent(plot(fcasts1, vars = 1))


# 6*_irf ---

# Ex-post irfs and methods
expect_silent(irf(run) <- irf(run, opt_irf1))
expect_silent(irfs1 <- irf(run))
expect_silent(irfs2 <- irf(run2, opt_irf2))
expect_silent(irfs3 <- irf(run2, opt_irf3))

expect_silent(print(irfs1))
expect_silent(print(summary(irfs1)))
expect_silent(print(fevd(run))) # Access
expect_silent(print(fevd(run2))) # Recalculates
expect_silent(print(fevd(irfs2))) # Recalculates
expect_silent(plot(irfs1, vars_res = 1, vars_imp = 1))


# 80_coda ---

# Get 'mcmc' object
expect_silent(coda::as.mcmc(run))


# 81_parallel ---

library("parallel")
cl <- makeCluster(2L)
expect_silent(
  tryCatch(run_par <- par_bvar(cl, n_runs = 2, data = data, lags = 2,
    priors = priors, mh = mh), finally = stopCluster(cl)))
expect_silent(plot(run, type = "full", vars = "lambda", chains = run_par))
expect_silent(coda::as.mcmc(run_par, vars = "lambda"))
expect_silent(BVAR:::chains_fit(run, run_par,
  Ms = TRUE, n_saves = TRUE, hypers = TRUE))


# 85_transform ---

x <- fred_md[c("RPI")]
expect_silent(fred_transform())
expect_silent(fred_transform(fred_qd[c("GDPC1", "GDPCTPI", "FEDFUNDS")]))
expect_silent(fred_transform(x, type = "fred_md"))
expect_silent(fred_transform(x, codes = 7, na.rm = FALSE))
expect_silent(fred_transform(x, type = "fred_md", lag = 2, scale = 50))

expect_silent(fred_code())
expect_silent(fred_code(c("GDPC1", "GDPCTPI", "FEDFUNDS")))
expect_silent(fred_code(c("RPI"), type = "fred_md"))
expect_silent(fred_code(c("GDPC1", "RPI"), table = TRUE))

expect_equivalent(fred_transform(x, code = 1), x)
expect_equivalent(fred_transform(x, code = 5, lag = 12, scale = 101),
  data.frame(diff(log(x[, 1]), lag = 12) * 101))

expect_message(fred_code("A"))
expect_message(fred_code("Temperature"))


# 9*_methods ---

expect_silent(print(run))

expect_silent(plot(run))
expect_silent(plot(run, type = "trace", vars = c("lambda")))
expect_silent(plot(run, type = "dens", vars_res = 1, vars_imp = 2))
expect_silent(plot(run, type = "fcast", vars = 1))
expect_silent(plot(run, type = "irf", vars_impulse = 1, vars_response = 1))
expect_silent(plot(predict(run, conf_bands = 0.5), vars = 1))
expect_silent(plot(predict(run, conf_bands = 0.25), vars = 3, area = TRUE))
expect_silent(plot(irf(run, conf_bands = 0.5),
  vars_impulse = 1, vars_response = 1))
expect_silent(plot(irf(run, conf_bands = 0.75), area = TRUE,
  vars_impulse = 3, vars_response = 3))


expect_silent(print(coef(run)))
expect_silent(print(coef(run, type = "mean")))
expect_silent(print(coef(run, conf_bands = 0.1)))
expect_silent(print(vcov(run)))
expect_silent(print(vcov(run, type = "mean")))

expect_silent(print(density(run, vars = 2)))
expect_silent(plot(density(run, vars = 2)))
expect_silent(independent_index(2, 4, 2))

expect_silent(print(fitted(run)))
expect_silent(print(residuals(run, conf_bands = 0.1)))
expect_silent(plot(residuals(run), vars = 1))

expect_silent(print(summary(run)))

expect_silent(print(companion(run)))
expect_silent(print(companion(run, type = "mean")))
expect_silent(print(companion(run, conf_bands = 0.1)))
