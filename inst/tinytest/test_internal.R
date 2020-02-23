
# Internal tests -----

set.seed(42)

# 11_input.R ---

expect_error(num_check(x = "1"))

expect_identical(num_check(0L), 0)

expect_error(int_check(c = 0.99, min = 1L))
expect_error(int_check(0L, min = 1L, max = Inf))

expect_identical(int_check(1.01), 1L)
expect_identical(int_check(1.99), 1L)

expect_error(auto_psi(cbind(rep(1, 100), rnorm(100))))
expect_silent(auto_psi(data, lags = 1))

# 12_aux.R ---

expect_equal(
  lag_var(matrix(1:4), lags = 2),
  matrix(c(0, 0, 2, 3, 0, 0, 1, 2), ncol = 2))

expect_equal(
  gamma_coef(2, 2),
  list("k" = 2.618034, "theta" = 1.236068), tol = 1e-6)

expect_equal(
  name_pars(c("a", "b", "psi", "c"), M = 2),
  c("a", "b", "psi1", "psi2", "c"))

expect_error(fill_ci("gray", "teal", 2))
expect_equal(fill_ci("gray", "teal", P = 3), c("teal", "gray", "teal"))

expect_equal(
  fill_ci_na(1:3, P = 2),
  cbind("x" = 1:3, NA))

expect_true(all(grepl("^[0-9a-fA-F]{2}$", transparance_hex(7))))
expect_true(all(grepl("^[0-9a-fA-F]{2}$", transparance_hex(120))))

expect_true(is_hex("#008080"))
expect_true(is_hex("#008080FF", alpha = TRUE))
expect_false(is_hex("008080"))
expect_false(is_hex("#0000"))
expect_false(is_hex("#008080FF", alpha = FALSE))

expect_error(pos_vars(vars = "a"))
expect_error(pos_vars(vars = "a", variables = c("x", "y")))
expect_equal(pos_vars(vars = NULL, M = 3), 1:3)
expect_equal(pos_vars(vars = c(2, 1), M = 3), c(1, 2))
expect_equal(pos_vars(c("a", "c"), variables = c("a", "b", "c")), c(1, 3))

expect_error(name_deps(c("a", "b"), M = 3))
expect_equal(name_deps(NULL, M = 3), c("var1", "var2", "var3"))
expect_equal(name_deps(c("a", "b"), M = 2), c("a", "b"))
expect_equal(
  name_expl(c("a", "b"), M = 2, lags = 2),
  c("constant", "a-lag1", "b-lag1", "a-lag2", "b-lag2"))

expect_equal(p_log_ig(5, 0.004, 0.004), -7.157927, tol = 1e-6)

expect_equal(
  get_beta_comp(matrix(1:12, nrow = 4), K = 4, M = 3, lags = 1),
  matrix(c(2:4, 6:8, 10:12), nrow = 3, byrow = TRUE))
expect_equal(
  get_beta_comp(matrix(1:21, nrow = 7), K = 7, M = 3, lags = 2),
  matrix(c(2:7, 9:14, 16:21, 1, rep(0, 6), 1, rep(0, 6), 1, rep(0, 3)),
    nrow = 6, byrow = TRUE))

expect_error(has_package("BSE"))
expect_equal(has_package("BVAR"), NULL)

expect_error(quantile_check(1))
expect_error(quantile_check(0))
expect_equal(quantile_check(0.5), 0.5)
expect_equal(quantile_check(0.1), c(0.1, 0.5, 0.9))
expect_equal(quantile_check(c(0.6, 0.4, 0.3)), c(0.3, 0.4, 0.5, 0.6, 0.7))
