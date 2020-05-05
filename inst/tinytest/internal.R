
# Internal tests -----

# 11_input.R ---

expect_error(BVAR:::num_check(x = "1"))

expect_identical(BVAR:::num_check(0L), 0)

expect_error(BVAR:::int_check(c = 0.99, min = 1L))
expect_error(BVAR:::int_check(0L, min = 1L, max = Inf))

expect_identical(BVAR:::int_check(1.01), 1L)
expect_identical(BVAR:::int_check(1.99), 1L)

# Fails, fits ARIMA, fits AR
expect_error(BVAR:::auto_psi(cbind(rep(1, 100), rnorm(100))))
expect_message(BVAR:::auto_psi(cbind(c(2, 1, 1, 3), c(1, 2, 0, 2)), lags = 1))
expect_silent(BVAR:::auto_psi(cbind(c(2, 1, 1, 2), c(1, 2, 0, 2)), lags = 1))


# 12_aux.R ---

expect_equal(
  BVAR:::lag_var(matrix(1:4), lags = 2),
  matrix(c(0, 0, 2, 3, 0, 0, 1, 2), ncol = 2))

expect_equal(
  BVAR:::gamma_coef(2, 2),
  list("k" = 2.618034, "theta" = 1.236068), tol = 1e-6)

expect_equal(
  BVAR:::name_pars(c("a", "b", "psi", "c"), M = 2),
  c("a", "b", "psi1", "psi2", "c"))

# Quite some cornercases here
expect_error(BVAR:::fill_ci("gray", y = "teal", P = 2))
expect_equal(
  BVAR:::fill_ci("gray", y = "teal", P = 3),
  c("teal", "gray", "teal"))
expect_equal(
  BVAR:::fill_ci_na(1:3, P = 2),
  cbind("x" = 1:3, NA))
expect_equal(
  BVAR:::fill_ci_na(1, P = 3),
  c(NA, 1, NA))
expect_equal(
  BVAR:::fill_ci_na(1:3, P = 5),
  cbind(NA, NA, "x" = 1:3, NA, NA))
expect_equal(
  BVAR:::fill_ci_col(x = integer(), y = "#008080", P = 5),
  c("#00808080", "#008080FF", "#008080FF", "#00808080"))

# Handpicked and automatic
expect_true(all(grepl("^[0-9a-fA-F]{2}$", BVAR:::alpha_hex(7))))
expect_true(all(grepl("^[0-9a-fA-F]{2}$", BVAR:::alpha_hex(120))))

expect_true(BVAR:::is_hex("#008080"))
expect_true(BVAR:::is_hex("#008080FF", alpha = TRUE))
expect_false(BVAR:::is_hex("008080"))
expect_false(BVAR:::is_hex("#0000"))
expect_false(BVAR:::is_hex("#008080FF", alpha = FALSE))

# Check numeric, character and missing
expect_error(BVAR:::pos_vars(vars = "a"))
expect_error(BVAR:::pos_vars(vars = "a", variables = c("x", "y")))
expect_equal(BVAR:::pos_vars(vars = NULL, M = 3), 1:3)
expect_equal(BVAR:::pos_vars(vars = c(2, 1), M = 3), c(2, 1))
expect_equal(
  BVAR:::pos_vars(c("a", "c"), variables = c("a", "b", "c")),
  c(1, 3))

expect_error(BVAR:::name_deps(c("a", "b"), M = 3))
expect_equal(BVAR:::name_deps(NULL, M = 3), c("var1", "var2", "var3"))
expect_equal(BVAR:::name_deps(c("a", "b"), M = 2), c("a", "b"))
expect_equal(
  BVAR:::name_expl(c("a", "b"), M = 2, lags = 2),
  c("constant", "a-lag1", "b-lag1", "a-lag2", "b-lag2"))
expect_equal(
  BVAR:::name_expl(NULL, M = 2, lags = 1),
  c("constant", "var1-lag1", "var2-lag1"))

expect_equal(BVAR:::p_log_ig(5, 0.004, 0.004), -7.157927, tol = 1e-6)

expect_equal(
  BVAR:::get_beta_comp(matrix(1:12, nrow = 4), K = 4, M = 3, lags = 1),
  matrix(c(2:4, 6:8, 10:12), nrow = 3, byrow = TRUE))
expect_equal(
  BVAR:::get_beta_comp(matrix(1:21, nrow = 7), K = 7, M = 3, lags = 2),
  matrix(c(2:7, 9:14, 16:21, 1, rep(0, 6), 1, rep(0, 6), 1, rep(0, 3)),
    nrow = 6, byrow = TRUE))

expect_error(BVAR:::has_package("BSE"))
expect_equal(BVAR:::has_package("BVAR"), NULL)

expect_error(BVAR:::quantile_check(1))
expect_error(BVAR:::quantile_check(0))
expect_equal(BVAR:::quantile_check(0.5), 0.5)
expect_equal(BVAR:::quantile_check(0.1), c(0.1, 0.5, 0.9))
expect_equal(
  BVAR:::quantile_check(c(0.6, 0.4, 0.3)),
  c(0.3, 0.4, 0.5, 0.6, 0.7))


# 13_mvtnorm.R ---

sigma <- matrix(c(1, 0.2, 0.1, 0, 1, 0.2, 0, 0, 1), nrow = 3)
sigma <- crossprod(sigma)

expect_silent(BVAR:::rmvn_proposal(1, mean = 0.5, sigma = list("values" = 1)))
expect_silent(BVAR:::rmvn_proposal(1, mean = 2, sigma = eigen(sigma)))
expect_silent(BVAR:::rmvn_inv(1, sigma_inv = solve(sigma), method = "eigen"))
expect_silent(BVAR:::rmvn_inv(1, sigma_inv = solve(sigma), method = "chol"))
expect_error(BVAR:::rmvn_inv(1, sigma_inv = solve(sigma), method = "svd"))


# 43_sur_soc.R ---

expect_silent(soc <- BVAR:::.add_soc(
  matrix(c(1, 2, 1, 2, 3, 3), ncol = 2), 2, 0.5))
expect_silent(sur <- BVAR:::.add_sur(
  matrix(c(1, 2, 1, 2, 3, 3), ncol = 2), 2, 0.5))
