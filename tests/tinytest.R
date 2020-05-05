
if(requireNamespace("tinytest", quietly = TRUE)) {

  set.seed(42)
  home <- length(unclass(packageVersion("BVAR"))[[1]]) == 4 # 0.0.0.9000

  if(home) {
    tinytest::test_package("BVAR", at_home = home, pattern = "^.*\\.[rR]$")
  } else {
    tinytest::test_package("BVAR", at_home = home, pattern = "^test.*\\.[rR]$")
  }
}
