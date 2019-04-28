to_source <- c(0:99)

sapply(list.files("R"), function(x) {
  if(any(stringr::str_detect(x, as.character(to_source))))
    source(paste0("R/", x))
})


# Simulated data ----------------------------------------------------------

data <- matrix(rnorm(20000), ncol = 5)
lags <- 5
n_draw <- 10000
n_burn <- 0
n_thin <- 1
fcast <- bv_fcast()
irf <- bv_irf()
verbose <- TRUE
priors <- bv_priors()
mh <- bv_mh()

run1 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors, mh, fcast, irf, verbose = TRUE)
run2 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors = bv_priors(hyper = c("lambda", "alpha")),
             mh, fcast, irf, verbose = TRUE)
run3 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors, mh, fcast, irf, verbose = TRUE)
run4 <- bvar(data, lags, n_draw, n_burn, n_thin = 3, irf = NULL, verbose = TRUE)
run5 <- bvar(data, lags, n_draw, n_burn, n_thin = 3, fcast = NULL, verbose = TRUE)
run6 <- bvar(data, lags, n_draw, n_burn, n_thin,
             priors = bv_priors(hyper = c("lambda", "alpha", "psi")),
             mh, fcast, irf, verbose = TRUE)

plot(run5)
bv_plot_trace(run1, "lambda", run2, run3)
bv_plot_irf(run1)
bv_plot_fcast(run1, ori = "h")
plot(run1$irf)
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

data_med_VAR <- fred_qd[, c("GDPC1", "GDPCTPI", "PCECC96", "GPDIC1",
                            "HOANBS", "COMPRNFB", "FEDFUNDS")]

data_med_VAR[5:nrow(data_med_VAR), -7] <- apply(log(data_med_VAR[, -7]), 2,
                                                diff, lag = 4) * 100
data_med_VAR <- data_med_VAR[5:nrow(data_med_VAR),]


add_soc <- function(Y, lags, par) {
  soc <- if(lags == 1) {diag(Y[1, ]) / par} else {
    diag(colMeans(Y[1:lags, ])) / par
  }
  Y_soc <- soc
  X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))

  return(list("Y" = Y_soc, "X" = X_soc))
}

soc <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)


add_sur <- function(Y, lags, par) {
  sur <- if(lags == 1) {Y[1, ] / par} else {
    colMeans(Y[1:lags, ]) / par
  }
  Y_sur <- sur
  X_sur <- c(1 / par, rep(sur, lags))

  return(list("Y" = Y_sur, "X" = X_sur))
}

sur <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_sur)


priors_v1 <- bv_priors(hyper = "auto",
                       "soc" = soc,
                       "sur" = sur,
                       mn = bv_mn(alpha = bv_alpha(mode = 2, max = 3)))

priors_v2 <- bv_priors(hyper = "full",
                       "soc" = soc,
                       "sur" = sur,
                       mn = bv_mn(lambda = bv_lambda(mode = 5, sd = 2)))
mh$acc_lower

run7 <- bvar(data_small_VAR, priors = priors_v1, lags = 5,
             irf = irf, mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run8 <- bvar(data_small_VAR, priors = priors_v1, lags = 12,
             irf = irf, mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)


run9 <- bvar(data_med_VAR, priors = priors_v1, lags = 5,
             irf = irf, mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run10 <- bvar(data_med_VAR, priors = priors_v2, lags = 5,
              irf = irf, mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)

bv_plot_irf(run7)
bv_plot_irf(run8)


bv_plot_irf(run9)
bv_plot_irf(run10)

bv_plot_irf(run9, vars_impulse = c(1, 7))

bv_plot_trace(run8, "lambda") # dafuq
bv_plot_trace(run9, "lambda")

run7$meta$accepted/run7$meta$n_draw
run8$meta$accepted/run8$meta$n_draw
run9$meta$accepted/run9$meta$n_draw
run10$meta$accepted/run10$meta$n_draw



signs <- matrix(c(1,1,-1,-1,1,-1,-1,1,1), nrow = 3)
irf_signs <- bv_irf(sign_restr = signs)

run8 <- bvar(data_small_VAR, priors = priors_dum, lags = 5, irf = irf_signs, verbose = TRUE)






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
