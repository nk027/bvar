### R code from vignette source '/home/nikolas/repos/bvar/vignettes/article.Rnw'

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
df <- fred_qd[, c("GDPC1", "PCECC96", "GPDIC1",
  "HOANBS", "GDPCTPI", "FEDFUNDS")]
df <- fred_transform(df, codes = c(4, 4, 4, 4, 4, 1))


###################################################
### code chunk number 4: plot_ts
###################################################
op <- par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(df)), df[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "Gross domestic product (GDP)")
plot(as.Date(rownames(df)), df[ , "PCECC96"], type = "l",
  xlab = "Time", ylab = "Consumption expenditure")
plot(as.Date(rownames(df)), df[ , "GPDIC1"], type = "l",
  xlab = "Time", ylab = "Private investment")
plot(as.Date(rownames(df)), df[ , "HOANBS"], type = "l",
  xlab = "Time", ylab = "Total hours worked (nfb)")
plot(as.Date(rownames(df)), df[ , "GDPCTPI"], type = "l",
  xlab = "Time", ylab = "GDP deflator")
plot(as.Date(rownames(df)), df[ , "FEDFUNDS"], type = "l",
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
### code chunk number 9: run
###################################################
run <- bvar(df, lags = 5, n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors, mh = mh, verbose = TRUE)


###################################################
### code chunk number 10: print_bvar
###################################################
print(run)


###################################################
### code chunk number 11: plot_bvar
###################################################
plot(run)


###################################################
### code chunk number 12: plot_dens
###################################################
plot(run, type = "dens", vars_response = "GDPC1",
  vars_impulse = c("GDPC1-lag1", "FEDFUNDS-lag2"))


###################################################
### code chunk number 13: fitted
###################################################
fitted(run, type = "mean")


###################################################
### code chunk number 14: plot_residuals
###################################################
plot(residuals(run, type = "mean"), vars = c("GDPC1", "PCECC96"))


###################################################
### code chunk number 15: irf
###################################################
opt_irf <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))


###################################################
### code chunk number 16: irf_plot
###################################################
plot(irf(run), vars_impulse = c("GDPC1", "FEDFUNDS"),
  area = TRUE, vars_response = c(1, 5:6))


###################################################
### code chunk number 17: predict
###################################################
predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))


###################################################
### code chunk number 18: predict_plot
###################################################
plot(predict(run), vars = c("GDPC1", "GDPCTPI", "FEDFUNDS"),
  area = TRUE, t_back = 25)


###################################################
### code chunk number 19: data_app
###################################################
data("fred_qd")
df_s <- fred_qd[, c("GDPC1", "GDPCTPI", "FEDFUNDS")]
fred_transform(df_s, type = "fred_qd")
df_s <- fred_transform(df_s, codes = c(5, 5, 1), lag = 4)


###################################################
### code chunk number 20: plot_ts_app
###################################################
op <- par(mfrow = c(1, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(df_s)), df_s[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(df_s)), df_s[ , "GDPCTPI"], type = "l",
  xlab = "Time", ylab = "Inflation")
plot(as.Date(rownames(df_s)), df_s[ , "FEDFUNDS"], type = "l",
  xlab = "Time", ylab = "Federal funds rate")
par(op)


###################################################
### code chunk number 21: run_app
###################################################
priors_s <- bv_priors(mn = bv_mn(b = 0))
run_s <- bvar(df_s, lags = 5, n_draw = 50000, n_burn = 25000,
  priors = priors_s, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))


###################################################
### code chunk number 22: custom_app
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
### code chunk number 23: priors_app
###################################################
soc <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)
priors_dum <- bv_priors(hyper = "auto", soc = soc)


###################################################
### code chunk number 24: coda_app
###################################################
library("coda")
run_mcmc <- as.mcmc(run_s, vars = "lambda")
geweke.diag(run_mcmc)


###################################################
### code chunk number 25: parallel_app
###################################################
library("parallel")
n_cores <- 3
cl <- makeCluster(n_cores)
runs <- par_bvar(cl = cl, data = df_s, lags = 5,
  n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors_s,
  mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))
stopCluster(cl)
runs_mcmc <- as.mcmc(run_s, vars = "lambda", chains = runs)
gelman.diag(runs_mcmc, autoburnin = FALSE)


###################################################
### code chunk number 26: parallel_plot_app
###################################################
plot(run_s, type = "full", vars = "lambda", chains = runs)


###################################################
### code chunk number 27: signs_app
###################################################
signs <- matrix(c(1, 1, 1, -1, 1, NA, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE,
  identification = TRUE, sign_restr = signs)
print(irf_signs)


###################################################
### code chunk number 28: irf_app
###################################################
irf(run_s) <- irf(run_s, irf_signs)


###################################################
### code chunk number 29: irf_plot_app
###################################################
plot(irf(run_s))


###################################################
### code chunk number 30: predict_app
###################################################
path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
predict(run_s) <- predict(run_s, horizon = 16,
  cond_path = path, cond_var = "FEDFUNDS")


###################################################
### code chunk number 31: predict_plot_app
###################################################
plot(predict(run_s), t_back = 16)


