### R code from vignette source 'article.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: setup
###################################################
set.seed(42)
library("BVAR")


###################################################
### code chunk number 3: data
###################################################
data("fred_qd")
x <- fred_qd[, c("GDPC1", "PCECC96", "GPDIC1",
  "HOANBS", "GDPCTPI", "FEDFUNDS")]
x <- fred_transform(x, codes = c(4, 4, 4, 4, 4, 1))


###################################################
### code chunk number 4: timeseries
###################################################
op <- par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(x)), x[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "Gross domestic product (GDP)")
plot(as.Date(rownames(x)), x[ , "PCECC96"], type = "l",
  xlab = "Time", ylab = "Consumption expenditure")
plot(as.Date(rownames(x)), x[ , "GPDIC1"], type = "l",
  xlab = "Time", ylab = "Private investment")
plot(as.Date(rownames(x)), x[ , "HOANBS"], type = "l",
  xlab = "Time", ylab = "Total hours worked (nfb)")
plot(as.Date(rownames(x)), x[ , "GDPCTPI"], type = "l",
  xlab = "Time", ylab = "GDP deflator")
plot(as.Date(rownames(x)), x[ , "FEDFUNDS"], type = "l",
  xlab = "Time", ylab = "Federal funds rate")
par(op)


###################################################
### code chunk number 5: minnesota
###################################################
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2), var = 1e07)


###################################################
### code chunk number 6: dummies
###################################################
soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


###################################################
### code chunk number 7: priors
###################################################
priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)


###################################################
### code chunk number 8: metropolis
###################################################
mh <- bv_metropolis(scale_hess = c(1, 0.005, 0.005), adjust_acc = TRUE,
  acc_lower = 0.25, acc_upper = 0.45, acc_change = 0.02)


###################################################
### code chunk number 9: bvar
###################################################
run <- bvar(x, lags = 5, n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors, mh = mh, verbose = TRUE)


###################################################
### code chunk number 10: print
###################################################
print(run)


###################################################
### code chunk number 11: trace_density
###################################################
plot(run)


###################################################
### code chunk number 12: betas
###################################################
plot(run, type = "dens", vars_response = "GDPC1",
  vars_impulse = c("GDPC1-lag1", "FEDFUNDS-lag2"))


###################################################
### code chunk number 13: fitted
###################################################
fitted(run, type = "mean")


###################################################
### code chunk number 14: residuals
###################################################
plot(residuals(run, type = "mean"), vars = c("GDPC1", "PCECC96"))


###################################################
### code chunk number 15: irf
###################################################
opt_irf <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))


###################################################
### code chunk number 16: irf_cholesky
###################################################
plot(irf(run), area = TRUE,
  vars_impulse = c("GDPC1", "FEDFUNDS"), vars_response = c(1, 5:6))


###################################################
### code chunk number 17: predict
###################################################
predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))


###################################################
### code chunk number 18: predict_unconditional
###################################################
plot(predict(run), area = TRUE, t_back = 24,
  vars = c("GDPC1", "GDPCTPI", "FEDFUNDS"))


###################################################
### code chunk number 19: app_data
###################################################
data("fred_qd")
y <- fred_qd[, c("GDPC1", "GDPCTPI", "FEDFUNDS")]
fred_transform(y, type = "fred_qd")
fred_transform()
y <- fred_transform(y, codes = c(5, 5, 1), lag = 4)


###################################################
### code chunk number 20: app_timeseries
###################################################
op <- par(mfrow = c(1, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(y)), y[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(y)), y[ , "GDPCTPI"], type = "l",
  xlab = "Time", ylab = "Inflation")
plot(as.Date(rownames(y)), y[ , "FEDFUNDS"], type = "l",
  xlab = "Time", ylab = "Federal funds rate")
par(op)


###################################################
### code chunk number 21: app_bvar
###################################################
priors_app <- bv_priors(mn = bv_mn(b = 0))
run_app <- bvar(y, lags = 5, n_draw = 50000, n_burn = 25000,
  priors = priors_app, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))


###################################################
### code chunk number 22: app_dummies
###################################################
add_soc <- function(Y, lags, par) {
  soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
    diag(colMeans(Y[1:lags, ])) / par
  }
  Y_soc <- soc
  X_soc <- cbind(rep(0, ncol(Y)),
    matrix(rep(soc, lags), nrow = ncol(Y)))
  return(list("Y" = Y_soc, "X" = X_soc))
}


###################################################
### code chunk number 23: app_priors
###################################################
soc <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)
bv_priors(hyper = "auto", soc = soc)


###################################################
### code chunk number 24: app_coda
###################################################
library("coda")
run_mcmc <- as.mcmc(run_app, vars = "lambda")
geweke.diag(run_mcmc)


###################################################
### code chunk number 25: app_parallel
###################################################
library("parallel")
n_cores <- 2
cl <- makeCluster(n_cores)
runs <- par_bvar(cl = cl, data = y, lags = 5,
  n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors_app, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))
stopCluster(cl)
runs_mcmc <- as.mcmc(runs, vars = "lambda")
gelman.diag(runs_mcmc, autoburnin = FALSE)


###################################################
### code chunk number 26: app_chains
###################################################
plot(runs, type = "full", vars = "lambda")


###################################################
### code chunk number 27: app_signs
###################################################
sr <- matrix(c(1, 1, 1, -1, 1, NA, -1, -1, 1), ncol = 3)
opt_signs <- bv_irf(horizon = 16, fevd = TRUE,
  identification = TRUE, sign_restr = sr)
print(opt_signs)


###################################################
### code chunk number 28: app_irf
###################################################
irf(run_app) <- irf(run_app, opt_signs)


###################################################
### code chunk number 29: app_irf_signs
###################################################
plot(irf(run_app))


###################################################
### code chunk number 30: app_predict
###################################################
path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
predict(run_app) <- predict(run_app, horizon = 16,
  cond_path = path, cond_var = "FEDFUNDS")


###################################################
### code chunk number 31: app_predict_conditional
###################################################
plot(predict(run_app), t_back = 16)


