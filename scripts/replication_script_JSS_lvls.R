#####
# Replication script for:
# "BVAR: Bayesian Vector Autoregressions with Hierarchical Prior Selection in R"


# Preliminaries -----------------------------------------------------------

# install.packages("BVAR") # Version 0.2.2.90000

set.seed(42)
library("BVAR")

# Loading and preparing data ----------------------------------------------

data("fred_qd")

df <- fred_qd[, c("GDPC1", "PCECC96", "GPDIC1",
  "HOANBS", "GDPCTPI", "FEDFUNDS")]
df <- fred_transform(df, type = "fred_qd",
  codes = c(4, 4, 4, 4, 4, 1))
df[, 1:4] <- df[, 1:4] * 4

# Plotting the time series
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


# `bvar()` setup ----------------------------------------------------------

# Setting up the Minnesota prior

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2), var = 1e07)


# Setting up dummy priors

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Putting the priors together

priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)


# Adjust the MH-step

mh <- bv_metropolis(scale_hess = c(1, 0.01, 0.01), adjust_acc = TRUE,
  acc_lower = 0.25, acc_upper = 0.45, acc_change = 0.02)


# Execute the model -------------------------------------------------------

run <- bvar(df, lags = 5, n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors, mh = mh, verbose = TRUE)


# Assessing results

print(run)

plot(residuals(run), vars = c("GDPC1", "PCECC96"))  # Residuals are partly quite off from 0 for other vars


# Hyperparameter plots

plot(run)

plot(run, vars_response = "GDPC1",
  vars_impulse = c("GDPC1-lag1", "FEDFUNDS-lag2"))


# Ex-post calculations ----------------------------------------------------

# Compute and plot IRFs ex-post

irfs <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, irfs, conf_bands = c(0.05, 0.16))
plot(irf(run), area = TRUE,
  vars_impulse = c("GDPC1", "FEDFUNDS"), vars_response = c(1:2, 5:6))


# Compute and plot unconditional forecast ex-post

predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))
plot(predict(run), area = TRUE,
  vars = c("GDPC1", "GDPCTPI", "FEDFUNDS"), t_back = 50)


# Appendices --------------------------------------------------------------

# A - Construction of custom dummy priors

add_soc <- function(Y, lags, par) {
  soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
    diag(colMeans(Y[1:lags, ])) / par
  }
  Y_soc <- soc
  X_soc <- cbind(rep(0, ncol(Y)),
    matrix(rep(soc, lags), nrow = ncol(Y)))
  return(list("Y" = Y_soc, "X" = X_soc))
}

soc <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)

priors_dum <- bv_priors(hyper = "auto", soc = soc)


# B - Data Transformation

data("fred_qd")
df_s <- fred_qd[, c("GDPC1", "GDPCTPI", "FEDFUNDS")]

fred_code(c("GDPC1", "GDPCTPI", "FEDFUNDS"), type = "fred_qd")

df_s <- fred_transform(df_s, type = "fred_qd", codes = c(5, 5, 1), lag = 4)

op <- par(mfrow = c(1, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(df_s)), df_s[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(df_s)), df_s[ , "GDPCTPI"], type = "l",
  xlab = "Time", ylab = "Inflation")
plot(as.Date(rownames(df_s)), df_s[ , "FEDFUNDS"], type = "l",
  xlab = "Time", ylab = "Federal funds rate")


# C - Convergence assessment and parallelization

priors_s <- bv_priors(mn = bv_mn(b = 0))

run_s <- bvar(df_s, lags = 5, n_draw = 50000, n_burn = 25000,
  priors = priors_s, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))

library("coda")
run_mcmc <- as.mcmc(run_s)
geweke.diag(run_mcmc)

library("parallel")
n_cores <- 3 # detectCores() - 1
cl <- makeCluster(n_cores)

runs <- par_bvar(cl = cl, data = df_s, lags = 5,
  n_draw = 50000, n_burn = 25000, n_thin = 1,
  priors = priors_s,
  mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))

stopCluster(cl)

plot(run_s, type = "full", vars = c("lambda"), chains = runs)

runs_mcmc <- as.mcmc(run_s, chains = runs)
gelman.diag(runs_mcmc, autoburnin = FALSE)


# D - Identification via sign restrictions and conditional forecasts

# D1 - Identification via sign restrictions

signs <- matrix(c(1, 1, 1, NA, 1, 1, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE,
  identification = TRUE, sign_restr = signs)

irf(run_s) <- irf(run_s, irf_signs)

print(irf(run_s))

plot(irf(run_s))


# D2 - Conditional forecasting with restricted FEDFUNDS

path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
predict(run_s) <- predict(run_s, horizon = 16,
  cond_path = path, cond_var = "FEDFUNDS")

plot(predict(run_s), t_back = 16)


# Fin ---------------------------------------------------------------------
