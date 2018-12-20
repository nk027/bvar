bv_priors <- function(hyper_pars,
  mn = bv_mn(lambda, alpha,
             var = 1e07, psi = "auto"),
  soc = bv_dummy(fnc = bv_soc), # Maybe leave this out and use it
  sur = bv_dummy(fnc = bv_sur), # to demonstrate adding dummies?
  ...) {

}

bv_mn <- function(
  lambda = c(0.2, 0.4, 0.0001, 5), # mode, sd, min, max
  alpha = c(2, 0.1, 0.5), # mode, min, max
  var = 1e07, psi = "auto") {

}

bv_dummy <- function(mode = 1, sd = 1,
                     min = 0.0001, max = 50,
                     fnc = NULL) {

  return(list(mode = mode, sd = sd,
              min = min, max = max,
              fnc = fnc))
}
