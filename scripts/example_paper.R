#####
# Working example

install.packages("BVAR")
library("BVAR")
set.seed(123)


# Loading and preparing data ----------------------------------------------

data("fred_qd")
data_VAR <- fred_qd[, c("GDPC1", "CPIAUCSL",
                        "PAYEMS", "INDPRO",
                        "SP500", "FEDFUNDS")]

## year-on-year changes
for(i in c(1, 2)) {
  data_VAR[5:nrow(data_VAR), i] <- diff(log(data_VAR[, i]), lag = 4) * 100
}

## quarter-on-quarter changes
for(i in c(3, 4, 5)) {
  data_VAR[2:nrow(data_VAR), i] <- diff(log(data_VAR[, i]), lag = 1) * 100
}

data_VAR <- data_VAR[5:nrow(data_VAR), ]

## Plotting it

par(mfrow = c(2, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
plot(as.Date(rownames(data_VAR)), data_VAR[ , "GDPC1"], type = "l",
     xlab = "Time", ylab = "GDP growth")
plot(as.Date(rownames(data_VAR)), data_VAR[ , "CPIAUCSL"], type = "l",
     xlab = "Time", ylab = "CPI inflation")
plot(as.Date(rownames(data_VAR)), data_VAR[ , "PAYEMS"], type = "l",
     xlab = "Time", ylab = "Non-farm empl.")
plot(as.Date(rownames(data_VAR)), data_VAR[ , "INDPRO"], type = "l",
     xlab = "Time", ylab = "Ind. production")
plot(as.Date(rownames(data_VAR)), data_VAR[ , "SP500"], type = "l",
     xlab = "Time", ylab = "S&P-500")
plot(as.Date(rownames(data_VAR)), data_VAR[ , "FEDFUNDS"], type = "l",
     xlab = "Time", ylab = "Fed Funds rate")


# Setting up Minnesota prior ----------------------------------------------

mn <- bv_mn(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
            alpha = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
            var = 1e07)


# Dummy priors ------------------------------------------------------------

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Putting priors together -------------------------------------------------

priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)



# Setting up impulse reponses ---------------------------------------------

irf  <- bv_irf(horizon = 12, fevd = TRUE, identification = TRUE)



# Setting up unconditional forecasts --------------------------------------

fcast <- bv_fcast(horizon = 12, conditional = FALSE)



# Setting up MH-step ------------------------------------------------------

mh <- bv_mh(scale_hess = 0.01, adjust_acc = TRUE,
            acc_lower = 0.25, acc_upper = 0.35, acc_change = 0.015)



# Running the model -------------------------------------------------------

run <- bvar(data_VAR, lags = 5, n_draw = 25000, n_burn = 10000, n_thin = 1,
            priors = priors, mh = mh, fcast = fcast, irf = irf,
            verbose = TRUE)


# Assessing results -------------------------------------------------------

print(run)


# Various plots -----------------------------------------------------------

par(mfrow = c(2, 1), mar = c(2, 2, 2, 2))
bv_plot_density(run, name = "lambda")
bv_plot_trace(run, name = "lambda")


plot(run)


bv_plot_fcast(run, conf_bands = 0.16,
              vars = c("GDPC1", "CPIAUCSL", "FEDFUNDS"),
              orientation = "horizontal")


bv_plot_irf(run, conf_bands = 0.16, vars_impulse = c("GDPC1", "FEDFUNDS"),
            vars_response = 1:5)
