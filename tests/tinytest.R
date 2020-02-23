
if(requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(packageVersion("BVAR"))[[1]]) == 4
  tinytest::test_package("BVAR", at_home = home)
}
