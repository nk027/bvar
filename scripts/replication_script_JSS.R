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
  "CES0600000007", "CPIAUCSL", "FEDFUNDS")]
df <- fred_transform(df, type = "fred_qd",
  codes = c(5, 5, 5, 5, 5, 1), lag = 4)

# Plotting the time series
op <- par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(df)), df[ , "GDPC1"], type = "l",
  xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(df)), df[ , "PCECC96"], type = "l",
  xlab = "Time", ylab = "Consumption exp. growth")
plot(as.Date(rownames(df)), df[ , "GPDIC1"], type = "l",
  xlab = "Time", ylab = "Private investment growth")
plot(as.Date(rownames(df)), df[ , "CES0600000007"], type = "l",
  xlab = "Time", ylab = "Avg. weekly hours changes")
plot(as.Date(rownames(df)), df[ , "CPIAUCSL"], type = "l",
  xlab = "Time", ylab = "CPI inflation")
plot(as.Date(rownames(df)), df[ , "FEDFUNDS"], type = "l",
  xlab = "Time", ylab = "Federal funds rate")


# `bvar()` setup ----------------------------------------------------------

# Setting up the Minnesota prior

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
  var = 1e07)


# Setting up dummy priors

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Putting the priors together

priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)


# # Setting up impulse reponses
#
# irfs  <- bv_irf(horizon = 12, fevd = TRUE, identification = TRUE)
#
#
# # Setting up unconditional forecasts
#
# fcasts <- bv_fcast(horizon = 12)


# Adjust the MH-step

mh <- bv_metropolis(scale_hess = 0.005, adjust_acc = TRUE,
  acc_lower = 0.25, acc_upper = 0.35, acc_change = 0.01)


# Execute the model -------------------------------------------------------

run <- bvar(df, lags = 5, n_draw = 25000, n_burn = 10000, n_thin = 1,
  priors = priors, mh = mh, fcast = NULL, irf = NULL, verbose = TRUE)


# Assessing results

summary(run)

print(run)

vcov(run)

plot(residuals(run), vars = c("GDPC1", "PCECC96"))

# Hyperparameter plots

plot(run)

plot(run, type = "full", vars = "lambda", mfrow = c(2, 1))


# Ex-post calculations ----------------------------------------------------

# Compute and plot IRFs ex-post

irfs <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, irfs, conf_bands = c(0.05, 0.16))
plot(irf(run), area = TRUE,
  vars_impulse = c("GDPC1", "FEDFUNDS"), vars_response = c(1:2, 5:6))


# Compute and plot unconditional forecast ex-post

predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))
plot(predict(run), area = TRUE,
  vars = c("GDPC1", "CPIAUCSL", "FEDFUNDS"), t_back = 3)


# Conditional forecast with restricted FEDFUNDS ex-post
path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2)
plot(predict(run, horizon = 16, cond_path = path, cond_var = "FEDFUNDS"),
     vars = c(1, 5:6), t_back = 3)


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


# B - Identification via sign restrictions

data("fred_qd")
df_s <- fred_qd[, c("GDPC1", "CPIAUCSL", "FEDFUNDS")]

df_s <- fred_transform(df_s, type = "fred_qd", codes = c(5, 5, 1), lag = 4)

signs <- matrix(c(1, 1, 1, NA, 1, 1, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE,
  identification = TRUE, sign_restr = signs)

run_signs <- bvar(df_s, lags = 5, n_draw = 25000, n_burn = 10000,
  priors = priors, mh = mh, irf = irf_signs)

print(run_signs)
print(irf(run_signs))

plot(irf(run_signs))


# C - Convergence assessment and parallelization

library("parallel")
n_cores <- 3 # detectCores() - 1
cl <- makeCluster(n_cores)

runs <- par_bvar(cl = cl, data = df, lags = 5,
  n_draw = 25000, n_burn = 10000, n_thin = 1,
  priors = bv_priors(soc = bv_soc(), sur = bv_sur()),
  mh = bv_mh(scale_hess = 0.005, adjust_acc = TRUE, acc_change = 0.01),
  irf = NULL, fcast = NULL)

stopCluster(cl)

plot(run, type = "full", vars = c("lambda", "soc"), chains = runs)

library("coda")
run_mcmc <- as.mcmc(runs[[1]])
geweke.diag(run_mcmc)

runs_mcmc <- as.mcmc(run, vars = c("lambda", "sur"), chains = runs)
gelman.diag(runs_mcmc, autoburnin = FALSE)


# Fin ---------------------------------------------------------------------
