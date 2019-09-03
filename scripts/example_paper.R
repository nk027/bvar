#####
# Working example for
# "BVAR: Bayesian Vector Autoregressions with Hierarchical Prior Selection in R"


# Preliminaries -----------------------------------------------------------

install.packages("BVAR") # Version 0.1.5
library("BVAR")
set.seed(123)


# Loading and preparing data ----------------------------------------------

data("fred_qd")
df <- fred_qd[, c("GDPC1", "INDPRO", "PAYEMS",
                  "CES0600000007", "CPIAUCSL", "FEDFUNDS")]

## year-on-year changes
for(i in c("GDPC1", "CPIAUCSL"))
  df[5:nrow(df), i] <- diff(log(df[, i]), lag = 4) * 100

## quarter-on-quarter changes
for(i in c("INDPRO", "PAYEMS", "CES0600000007"))
  df[2:nrow(df), i] <- diff(log(df[, i]), lag = 1) * 100

df <- df[5:nrow(df), ]

## Plotting the series
pdf("../time_series_ovw.pdf", width = 10, height = 6)
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
dev.off()


# Setting up Minnesota prior ----------------------------------------------

mn <- bv_mn(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
            alpha = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
            var = 1e07)



# Setting up dummy priors -------------------------------------------------

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Putting priors together -------------------------------------------------

priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)


# Setting up impulse reponses ---------------------------------------------

irf  <- bv_irf(horizon = 12, fevd = TRUE, identification = TRUE)


# Setting up unconditional forecasts --------------------------------------

fcast <- bv_fcast(horizon = 12, conditional = FALSE)


# Setting up MH-step ------------------------------------------------------

mh <- bv_mh(scale_hess = 0.005, adjust_acc = TRUE,
            acc_lower = 0.25, acc_upper = 0.35, acc_change = 0.02)


# Running the model -------------------------------------------------------

run <- bvar(df, lags = 5, n_draw = 25000, n_burn = 10000, n_thin = 1,
            priors = priors, mh = mh, fcast = fcast, irf = irf,
            verbose = TRUE)


# Assessing results --------------------------------------------------------

summary(run)

# Various plots ------------------------------------------------------------

pdf("../plots_all.pdf", width = 10, height = 12)
plot(run)
dev.off()

pdf("../plots_lambda.pdf", width = 10, height = 5)
plot(run, type = "full", vars = "lambda", mfrow = c(2, 1))
dev.off()

pdf("../plots_fcast.pdf", width = 10, height = 3)
plot(run$fcast, vars = c("GDPC1", "CPIAUCSL", "FEDFUNDS"),
     orientation = "h")
dev.off()

pdf("../plots_irf.pdf", width = 10, height = 8)
plot(run$irf, vars_impulse = c("GDPC1", "FEDFUNDS"),
            vars_response = c(1:5))
dev.off()

# using predict() for ex-post computation of different confidence bands

fcast_005 <- predict(run, conf_bands = 0.05)

pdf("../plots_fcast_005.pdf", width = 10, height = 3)
plot(fcast_005, vars = c("GDPC1", "CPIAUCSL", "FEDFUNDS"),
     orientation = "h")
dev.off()

irf_005 <- irf(run, conf_bands = 0.05)

pdf("../plots_irf005.pdf", width = 10, height = 8)
plot(irf_005, vars_impulse = c("GDPC1", "FEDFUNDS"),
     vars_response = c(1:4, 5))
dev.off()

# Calculate FEVDs ---------------------------------------------------------

fevd(run)
print(fevd(run, conf_bands = 0.16), complete = TRUE)

# Appendices --------------------------------------------------------------

# A - Constructing a dummy prior ------------------------------------------

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


# B - Signs restrictions as a means of identification ---------------------

data("fred_qd")
df_small <- fred_qd[, c("GDPC1", "CPIAUCSL", "FEDFUNDS")]
for(i in c("GDPC1", "CPIAUCSL"))
  df_small[5:nrow(df_small), i] <- diff(log(df_small[, i]), lag = 4) * 100

df_small <- df_small[5:nrow(df_small), ]


signs <- matrix(c(1, 1, 1, 0, 1, 1, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE,
                    identification = TRUE, sign_restr = signs)
run_signs <- bvar(df_small, lags = 5, n_draw = 25000, n_burn = 10000,
                  priors = priors, mh = mh, fcast = fcast, irf = irf_signs)
print(run_signs)
plot(run_signs$irf)

pdf("../irf_signs.pdf", width = 10, height = 10)
plot(run_signs$irf)
dev.off()


# C - Convergence assessment and parallelization --------------------------

library("coda")
run_mcmc <- as.mcmc(run)
geweke.diag(run_mcmc)

library("parallel")
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
bvars <- parLapply(cl, list(df, df, df),
                   function(x) {
                     library("BVAR")
                     bvar(x, lags = 5,
                          n_draw = 25000, n_burn = 10000, n_thin = 1,
                          priors = bv_priors(soc = bv_soc(), sur = bv_sur()),
                          mh = bv_mh(scale_hess = 0.005, adjust_acc = TRUE,
                                     acc_change = 0.02),
                          irf = bv_irf(), fcast = bv_fcast(), verbose = FALSE)
                   })
stopCluster(cl)

plot(run, type = "density", vars = "lambda", chains = bvars)

pdf("../lambda_multiple.pdf", width = 10, height = 4)
plot(run, type = "density", vars = "lambda", chains = bvars)
dev.off()

runs_mcmc <- as.mcmc(run, chains = bvars)
gelman.diag(runs_mcmc, autoburnin = FALSE)
