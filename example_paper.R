#####
# Working example

install.packages("BVAR")

library(BVAR)



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



# Adding dummy priors -----------------------------------------------------

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

