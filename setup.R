to_source <- c(0:99)

sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})


# Simulated data ----------------------------------------------------------

data <- matrix(rnorm(20000), ncol = 5)
lags <- 2
n_draw <- 10000
n_burn <- 0
n_thin <- 1
fcast <- bv_fcast()
irf <- bv_irf()
verbose <- TRUE
priors <- bv_priors()
metropolis <- bv_metropolis()

run1 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors, metropolis, fcast, irf, verbose = TRUE)
run2 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors = bv_priors(hyper = c("lambda", "alpha")),
             metropolis, fcast, irf, verbose = TRUE)
run3 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors, metropolis, fcast, irf, verbose = TRUE)
run4 <- bvar(data, lags, n_draw, n_burn, n_thin = 3, irf = NULL, verbose = TRUE)
run5 <- bvar(data, lags, n_draw, n_burn, n_thin = 3, fcast = NULL, verbose = TRUE)
run6 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors = bv_priors(hyper = c("lambda", "alpha", "psi")),
             metropolis, fcast, irf, verbose = TRUE)

plot(run5)
bv_plot_trace(run1, "lambda", run2, run3)
bv_plot_irf(run1)
bv_plot_fcast(run1)
plot(run2$irf)
plot(run4$fcast)

run1
run1$irf
run1$fcast


# Real data ---------------------------------------------------------------

data("fred_qd")

data_small_VAR <- fred_qd[, c("CPIAUCSL", "UNRATE", "FEDFUNDS")]

data_small_VAR[5:nrow(data_small_VAR), 1] <- diff(log(data_small_VAR[, 1]),
                                                  lag = 4) * 100
data_small_VAR <- data_small_VAR[5:nrow(data_small_VAR),]
run7 <- bvar(data_small_VAR, lags = 5, irf = irf, verbose = TRUE)

signs <- matrix(c(1,1,-1,-1,1,-1,-1,1,1), nrow = 3)
irf_signs <- bv_irf(sign_restr = signs)
run8 <- bvar(data_small_VAR, lags = 5, irf = irf_signs, verbose = TRUE)

bv_plot_irf(run7)
bv_plot_irf(run8)



# data_med_VAR <- fred_qd[, c("RGDP", "PGDP", "Cons", "GPDInv",
#                                   "Emp..Hours", "Real.Comp.Hour", "FedFunds")]
# data_large_VAR <- fred_qd[, c("RGDP", "PGDP", "CPI.ALL",
#                                     "Com..spot.price..real.", "IP..total" ,
#                                     "Emp..total", "U..all", "Cons", "Res.Inv",
#                                     "NonResInv", "PCED","PGPDI", "Capacity.Util",
#                                     "Consumer.expect", "Emp..Hours", "Real.Comp.Hour",
#                                     "FedFunds", "X1.yr.T.bond", "X5.yr.T.bond",
#                                     "S.P.500", "Ex.rate..avg", "M2")]
#
# data_med_VAR[, -7] <- apply(data_med_VAR[, -7], 2, log) * 4
# data_med_VAR[, 7]  <- data_med_VAR[, 7] / 100
# data_med_VAR <- na.omit(data_med_VAR)
#
# data_large_VAR[, -c(7, 13, 14, 17, 18, 19)] <-
#   apply(data_large_VAR[, -c(7, 13, 14, 17, 18, 19)], 2, log) * 4
# data_large_VAR[, c(7, 13, 14, 17, 18, 19)]  <-
#   data_large_VAR[, c(7, 13, 14, 17, 18, 19)] / 100
# data_large_VAR <- na.omit(data_large_VAR)
