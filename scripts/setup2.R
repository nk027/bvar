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
bv_plot_fcast(run1)
plot(run2$irf)
plot(run4$fcast)

run1
run1$irf
run1$fcast


# Real data ---------------------------------------------------------------

data("fred_qd")

data_small_VAR <- fred_qd[, c("GDPC1", "GDPCTPI", "FEDFUNDS")]

data_small_VAR1 <- data_small_VAR
data_small_VAR2 <- data_small_VAR

data_small_VAR1[, 1:2] <- log(data_small_VAR1[, 1:2])
data_small_VAR1 <- data_small_VAR1[5:nrow(data_small_VAR1), ]
data_small_VAR1 <- data_small_VAR1[5:which(rownames(data_small_VAR1) ==  "2008-12-01"), ]


# data_small_VAR2[2:nrow(data_small_VAR2), 1:2] <-  apply(log(data_small_VAR2[, 1:2]),
#                                                    2, diff, lag = 2)

data_small_VAR2[3:nrow(data_small_VAR2), 1] <- diff(log(data_small_VAR2[, 1]), lag = 2) * 100
data_small_VAR2[3:nrow(data_small_VAR2), 2] <- diff(log(data_small_VAR2[, 2]), lag = 2) * 100
data_small_VAR2 <- data_small_VAR2[3:nrow(data_small_VAR2), ]

data_small_VAR2 <- data_small_VAR2[5:which(rownames(data_small_VAR2) ==  "2008-12-01"), ]



data_med_VAR <- fred_qd[, c("GDPC1", "GDPCTPI", "PCECC96", "GPDIC1",
                            "HOANBS", "COMPRNFB", "FEDFUNDS")]
data_med_VAR1 <- data_med_VAR
data_med_VAR2 <- data_med_VAR

data_med_VAR1[, -7] <- log(data_med_VAR1[, -7]) * 4
data_med_VAR1 <- data_med_VAR1[5:which(rownames(data_med_VAR1) == "2008-12-01"), ]

data_med_VAR2[5:nrow(data_med_VAR2), 1:6] <- apply(log(data_med_VAR2[, 1:6]),
                                                 2, diff, lag = 4) * 100
data_med_VAR2 <- data_med_VAR2[5:which(rownames(data_med_VAR2) == "2008-12-01"), ]



data_large_VAR <- fred_qd[, c("GDPC1", "PCECC96", "COMPRNFB","PAYEMS", "SRVPRD",
                              "HOANBS", "INDPRO", "CUMFNS", "UMCSENTx",  "PRFIx",
                              "PNFIx", "GDPCTPI", "CPIAUCSL", "PCECTPI",
                              "GPDICTPI", "FEDFUNDS", "PPIACO", "GS1", "GS5",
                              "SP500", "EXUSUKx", "M2REAL")]
data_large_VAR1 <- data_large_VAR
data_large_VAR2 <- data_large_VAR

data_large_VAR1[, -c(8, 9, 16, 18, 19, 21)] <- log(
  data_large_VAR1[, -c(8, 9, 16, 18, 19, 21)]) * 4
data_large_VAR1 <- data_large_VAR1[5:which(rownames(data_large_VAR1) == "2008-12-01"), ]

data_large_VAR2[5:nrow(data_large_VAR2), -c(8, 9, 16, 18, 19)] <- apply(log(
  data_large_VAR2[, -c(8, 9, 16, 18, 19)]), 2, diff, lag = 4) * 100
data_large_VAR2 <- data_large_VAR2[5:which(rownames(data_large_VAR2) == "2008-12-01"), ]


# data_large_VAR[, -c(7, 13, 14, 17, 18, 19)] <-
#   apply(data_large_VAR[, -c(7, 13, 14, 17, 18, 19)], 2, log) * 4
# data_large_VAR[, c(7, 13, 14, 17, 18, 19)]  <-
#   data_large_VAR[, c(7, 13, 14, 17, 18, 19)] / 100
# data_large_VAR <- na.omit(data_large_VAR)



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
                       "sur" = sur)

priors_v2 <- bv_priors(hyper = "full",
                       "soc" = soc,
                       "sur" = sur)

priors_v3 <- bv_priors(hyper = c("alpha", "lambda", "soc", "sur"),
                       "soc" = soc,
                       "sur" = sur)

run6 <- bvar(data_small_VAR1, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run7 <- bvar(data_small_VAR2, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)


plot(run6)
bv_plot_irf(run6)
bv_plot_trace(run6, "lambda")
bv_plot_density(run6, "lambda")

plot(run7)
bv_plot_irf(run7)
bv_plot_trace(run7, "lambda")
bv_plot_density(run7, "lambda")


run8 <- bvar(data_med_VAR1, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run9 <- bvar(data_med_VAR2, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)

plot(run8)
bv_plot_irf(run8, vars_impulse = 7, vars_response = c(1:6))
bv_plot_trace(run8, "lambda")
bv_plot_density(run8, "lambda")

plot(run9)
bv_plot_irf(run9, vars_impulse = 7, vars_response = c(1:6))
bv_plot_trace(run9, "lambda")
bv_plot_density(run9, "lambda")


run10 <- bvar(data_large_VAR1, n_draw = 10000, n_burn = 5000, priors = priors_v1, lags = 5,
              irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run11 <- bvar(data_large_VAR2, n_draw = 10000, n_burn = 5000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)

plot(run10)
bv_plot_irf(run10, vars_impulse = 16, vars_response = c(1:5))
bv_plot_trace(run10, "lambda")
bv_plot_density(run10, "lambda")

plot(run11)
bv_plot_irf(run11, vars_impulse = 16, vars_response = c(1:5))
bv_plot_trace(run11, "lambda")
bv_plot_density(run11, "lambda")


run6$meta$accepted/(run6$meta$n_draw-run6$meta$n_burn)
run7$meta$accepted/(run7$meta$n_draw-run7$meta$n_burn)
run8$meta$accepted/(run8$meta$n_draw-run8$meta$n_burn)
run9$meta$accepted/(run9$meta$n_draw-run9$meta$n_burn)
run10$meta$accepted/(run10$meta$n_draw-run10$meta$n_burn)
run11$meta$accepted/(run11$meta$n_draw-run11$meta$n_burn)


rm(list=lsf.str())
require(BVAR)


run12 <- bvar(data_small_VAR1, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run13 <- bvar(data_small_VAR2, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)


plot(run12)
bv_plot_irf(run12)
bv_plot_trace(run12, "lambda")
bv_plot_density(run12, "lambda")

plot(run13)
bv_plot_irf(run13)
bv_plot_trace(run13, "lambda")
bv_plot_density(run13, "lambda")


run14 <- bvar(data_med_VAR1, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run15 <- bvar(data_med_VAR2, n_draw = 20000, n_burn = 10000, priors = priors_v1, lags = 5,
             irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)

plot(run14)
bv_plot_irf(run14, vars_impulse = 7, vars_response = c(1:6))
bv_plot_trace(run14, "lambda")
bv_plot_density(run14, "lambda")

plot(run15)
bv_plot_irf(run15, vars_impulse = 7, vars_response = c(1:6))
bv_plot_trace(run15, "lambda")
bv_plot_density(run15, "lambda")


run16 <- bvar(data_large_VAR1, n_draw = 10000, n_burn = 5000, priors = priors_v1, lags = 5,
              irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)
run17 <- bvar(data_large_VAR2, n_draw = 10000, n_burn = 5000, priors = priors_v1, lags = 5,
              irf = bv_irf(), mh = bv_mh(adjust_acc = TRUE), verbose = TRUE)

plot(run16)
bv_plot_irf(run16, vars_impulse = 16, vars_response = c(1:5))
bv_plot_trace(run16, "lambda")
bv_plot_density(run16, "lambda")

plot(run17)
bv_plot_irf(run17, vars_impulse = 16, vars_response = c(1:5))
bv_plot_trace(run17, "lambda")
bv_plot_density(run17, "lambda")


run12$meta$accepted/(run12$meta$n_draw-run6$meta$n_burn)
run13$meta$accepted/(run13$meta$n_draw-run7$meta$n_burn)
run14$meta$accepted/(run14$meta$n_draw-run8$meta$n_burn)
run15$meta$accepted/(run15$meta$n_draw-run9$meta$n_burn)
run16$meta$accepted/(run16$meta$n_draw-run10$meta$n_burn)
run17$meta$accepted/(run17$meta$n_draw-run11$meta$n_burn)








signs <- matrix(c(1,1,-1,-1,1,-1,-1,1,1), nrow = 3)
irf_signs <- bv_irf(sign_restr = signs)

run8 <- bvar(data_small_VAR, priors = priors_dum, lags = 5, irf = irf_signs, verbose = TRUE)









