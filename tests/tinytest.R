
if(requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(packageVersion("BVAR"))[[1]]) == 4

  set.seed(42)

  if(interactive()) { # Test API and internals
    tinytest::test_all(at_home = TRUE, pattern = "^.*\\.[rR]$")
  } else { # Test the API
    tinytest::test_package("BVAR",
      at_home = home, pattern = "^test.*\\.[rR]$")
  }
}
