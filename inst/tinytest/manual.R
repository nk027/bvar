
# Manual tests -----

# 13_mvtnorm.R ---

sigma <- matrix(c(1, 0.2, 0.1, 0, 1, 0.2, 0, 0, 1), nrow = 3)
sigma <- crossprod(sigma)

uvt_1 <- BVAR:::rmvn_proposal(1000, mean = 2, sigma = list("values" = 5))
uvt_2 <- rnorm(1000, mean = 2, sd = 5)

# Kolmogorov-Smirnov Test for univariate distributions
expect_true(ks.test(uvt_1, uvt_2)[["p.value"]] >= 0.1)

mvt_1 <- BVAR:::rmvn_proposal(1000, mean = 2, sigma = eigen(sigma))
mvt_2 <- mvtnorm::rmvnorm(1000, mean = rep(2, 3L), sigma = sigma)

mvt_3 <- BVAR:::rmvn_inv(1000, sigma_inv = solve(sigma), method = "eigen")
mvt_4 <- BVAR:::rmvn_inv(1000, sigma_inv = solve(sigma), method = "chol")
mvt_5 <- mvtnorm::rmvnorm(1000, mean = rep(0, 3L), sigma = sigma)

# Cramer test for multivariate distributions
if(requireNamespace("cramer", quietly = TRUE)) {
  expect_true(cramer::cramer.test(mvt_1, mvt_2)[["p.value"]] >= 0.1)
  expect_true(cramer::cramer.test(mvt_3, mvt_4)[["p.value"]] >= 0.1)
  expect_true(cramer::cramer.test(mvt_3, mvt_5)[["p.value"]] >= 0.1)
}
