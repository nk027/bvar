
if(requireNamespace("tinytest", quietly = TRUE)) {

  set.seed(42)
  home <- length(unclass(packageVersion("BVAR"))[[1]]) == 4

  if(interactive()) { # Test API and internals
    tinytest::test_all(at_home = home, pattern = "^.*\\.[rR]$")
  } else { # Test the API
    tinytest::test_package("BVAR",
      at_home = home, pattern = "^test.*\\.[rR]$")
  }
}
