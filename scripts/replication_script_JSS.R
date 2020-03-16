#####
# Replication script for:
# "BVAR: Bayesian Vector Autoregressions with Hierarchical Prior Selection in R"


# Preliminaries -----------------------------------------------------------

# install.packages("BVAR") # Version 0.2.1

set.seed(42)
library("BVAR")


# Loading and preparing data ----------------------------------------------

data("fred_qd")


df <- fred_qd[, c("GDPC1", "INDPRO", "PAYEMS",
  "CES0600000007", "CPIAUCSL", "FEDFUNDS")]
 df <- fred_transform(df, type = "fred_qd")

# year-to-year changes
for(i in c("GDPC1", "CPIAUCSL"))
  df[5:nrow(df), i] <- diff(log(df[, i]), lag = 4) * 100

# quarter-to-quarter changes
for(i in c("INDPRO", "PAYEMS", "CES0600000007"))
  df[2:nrow(df), i] <- diff(log(df[, i]), lag = 1) * 100

df <- df[5:nrow(df), ]


# Plotting the time series
op <- par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(df)), df[ , "GDPC1"], type = "l",
     xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(df)), df[ , "INDPRO"], type = "l",
     xlab = "Time", ylab = "Ind. production growth")
plot(as.Date(rownames(df)), df[ , "PAYEMS"], type = "l",
     xlab = "Time", ylab = "Non-farm empl. changes")
plot(as.Date(rownames(df)), df[ , "CES0600000007"], type = "l",
     xlab = "Time", ylab = "Avg. weekly hours changes")
plot(as.Date(rownames(df)), df[ , "CPIAUCSL"], type = "l",
     xlab = "Time", ylab = "CPI inlation")
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


# Setting up impulse reponses

irfs  <- bv_irf(horizon = 12, fevd = TRUE, identification = TRUE)


# Turning off forecasts

fcasts <- NULL


# Adjust the MH-step

mh <- bv_metropolis(scale_hess = 0.005, adjust_acc = TRUE,
  acc_lower = 0.25, acc_upper = 0.35, acc_change = 0.05)


# Execute the model -------------------------------------------------------

run <- bvar(df, lags = 5, n_draw = 25000, n_burn = 10000, n_thin = 1,
  priors = priors, mh = mh, fcast = fcasts, irf = irfs, verbose = TRUE)


# Assessing results

summary(run)


# Hyperparameter plots

plot(run)

plot(run, type = "full", vars = "lambda", mfrow = c(2, 1))


# IRF plots

plot(irf(run), vars_impulse = c("GDPC1", "FEDFUNDS"),
  vars_response = c(1:5))


# Get FEVD values

fevd(run)


# Ex-post calculations ----------------------------------------------------

# Do conditional forecast with restricted FEDFUNDS ex-post
path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2)
plot(predict(run, horizon = 12, cond_path = path, cond_var = "FEDFUNDS"),
     vars = c(1:2, 5:6), area = TRUE, t_back = 10)

# Plot adjusted IRFs ex-post
plot(irf(run, conf_bands = c(0.05, 0.16), horizon = 20, fevd = FALSE),
  vars_impulse = c("GDPC1", "FEDFUNDS"), vars_response = c(1:3, 5:6),
  area = TRUE)


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
df_small <- fred_qd[, c("GDPC1", "CPIAUCSL", "FEDFUNDS")]

df_small <- fred_transform(df_small, type = "fred_qd", lag = 1)

for(i in c("GDPC1", "CPIAUCSL"))
  df_small[2:nrow(df_small), i] <- diff(log(df_small[, i]), lag = 1) * 100

df_small <- df_small[2:nrow(df_small), ]


signs <- matrix(c(1, 1, 1, NA, 1, 1, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE,
  identification = TRUE, sign_restr = signs)

run_signs <- bvar(df_small, lags = 5, n_draw = 25000, n_burn = 10000,
  priors = priors, mh = mh, fcast = fcasts, irf = irf_signs)

print(run_signs)
print(irf(run_signs))

plot(irf(run_signs), area = TRUE)


# C - Convergence assessment and parallelization

library("parallel")
n_cores <- 3 # detectCores() - 1
cl <- makeCluster(n_cores)

runs <- par_bvar(cl = cl, data = df, lags = 5,
  n_draw = 25000, n_burn = 10000, n_thin = 1,
  priors = bv_priors(soc = bv_soc(), sur = bv_sur()),
  mh = bv_mh(scale_hess = 0.005, adjust_acc = TRUE, acc_change = 0.02),
  irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE),
  fcast = NULL)

stopCluster(cl)

plot(run, type = "full", vars = c("lambda", "soc"), chains = runs)

library("coda")
run_mcmc <- as.mcmc(runs[[1]], vars = c("lambda", "soc"))
geweke.diag(run_mcmc)


runs_mcmc <- as.mcmc(runs, vars = c("lambda", "sur"))
gelman.diag(runs_mcmc, autoburnin = FALSE)


# Fin ---------------------------------------------------------------------
