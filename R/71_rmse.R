
rmse <- \(x) {
  apply(resid(x), 2, \(r) sqrt(sum(r^2) / length(r)))
}

lps <- \(x) {
  fit <- array(NA, dim = c(1000, x$meta$N, x$meta$M))
  for(i in seq(1000)) {
    fit[i, , ] <- x$meta$X %*% x$beta[i, , ]
  }
  mu <- apply(fit, c(2, 3), mean)
  sd <- apply(fit, c(2, 3), sd)
  lps <- matrix(NA, x$meta$N, x$meta$M)
  for(j in seq(x$meta$M)) {
    lps[, j] <- dnorm(x$meta$Y[, j] - mu[, j], sd = sd[, j], log = TRUE)
  }
}
