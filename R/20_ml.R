bv_ml <- function(
  pars,
  priors) {

  # outside bounds?


  # Priors ------------------------------------------------------------------

  omega <- vector("numeric", 1 + M * lags)
  omega[1] <- priors$var
  for(i in 1:lags) {
    omega[seq(2 + M * (i - 1), 1 + i * M)] <-
      pars["lambda"] ^ 2 / i ^ pars["alpha"] / pars[names(pars) == "psi"]
  }

  # Dummy priors


  # Calc --------------------------------------------------------------------

  # Likelihood


  # Posteriors --------------------------------------------------------------




}


log_mlike <- function(pars = NULL,
                      priors,
                      Y, X, lags = NULL,
                      prior_coef,
                      draw = FALSE) {

  # Robustness --------------------------------------------------------------

  if(is.null(lags)) {lags <- (ncol(X) - 1) / ncol(Y)}
  if(length(priors$hyperpars == 1)) {
    names(pars) <- priors$hyperpars
  }

  # Check the Minnesota prior
  if(is.null(priors$lambda) || is.null(priors$alpha) || is.null(priors$var) ||
     is.null(priors$psi)) {stop("Minnesota prior not specified.")}

  if(!all(names(priors[!names(priors) %in%
                       c("hyperpars", "alpha", "psi", "var", "b")]) %in%
          names(prior_coef))) {stop("prior_coef is missing values")}

  # To-do: Check dummy priors (mode/par, fnc)

  # Set log_mlike to -1e18 if the draw is out of the boundaries
  if(any(sapply(priors$hyperpars, function(x) {
    any(priors[[x]]$min >= pars[which(names(pars) == x)]) ||
      any(pars[which(names(pars) == x)] >= priors[[x]]$max)
  }))) {return(list("log_mlike" = -1e18))}


  # Setup -------------------------------------------------------------------

  K <- ncol(X)
  M <- ncol(Y)

  prior_names <- names(priors[which(!names(priors) %in%
                                      c("hyperpars", "psi", "var", "b"))])

  if(is.null(pars)) {
    pars <- rep(NA, length(priors) - 3)
    names(pars) <- prior_names
  }
  for(name in prior_names) {
    if(is.na(pars[name])) {pars[name] <- priors[[name]]$mode}
  }
  if(is.na(pars["psi"])) {
    psi <- priors$psi$mode
    names(psi) <- rep("psi", length(psi))
    pars <- c(pars, psi)
  } else {psi <- pars[names(pars) == "psi"]}
  psi <- diag(psi)


  # Priors ------------------------------------------------------------------

  omega <- vector("numeric", 1 + M * lags)
  omega[1] <- priors$var
  for(i in 1:lags) {
    omega[seq(2 + M * (i - 1), 1 + i * M)] <-
      pars["lambda"] ^ 2 / i ^ pars["alpha"] / pars[names(pars) == "psi"]
  }

  # Dummy-observation priors
  dmy_priors <- names(priors)[!names(priors) %in%
                                c("hyperpars",
                                  "lambda", "alpha", "psi", "var", "b")]

  dummies <- lapply(dmy_priors, function(x) {
    if(x %in% names(pars)) {
      par <- pars[x]
    } else {par <- priors[[x]]$mode}

    priors[[x]]$fnc(Y = Y, lags = lags, par = par)
  })

  Y <- rbind(do.call(rbind, lapply(dummies, function(x) {
    matrix(x$Y, ncol = M)
  })), Y)

  X <- rbind(do.call(rbind, lapply(dummies, function(x) {
    matrix(x$X, ncol = K)
  })), X)
  N <- nrow(Y)


  # OLS ---------------------------------------------------------------------

  omega_inv <- diag(1 / omega)
  beta_hat <- solve(crossprod(X) + omega_inv) %*%
    (crossprod(X, Y) + omega_inv %*% priors$b)
  sse <- crossprod(Y - X %*% beta_hat)

  psi_inv <- solve(sqrt(psi))
  omega_ml <- diag(sqrt(omega)) %*% crossprod(X) %*% diag(sqrt(omega))
  psi_ml <- psi_inv %*%
    (sse + t(beta_hat - priors$b) %*% omega_inv %*% (beta_hat - priors$b)) %*%
    psi_inv

  # Eigenvalues + 1 as another way of computing the determinants in A.14
  omega_ml_ev <- Re(eigen(omega_ml, only.values = TRUE)$values)
  omega_ml_ev[omega_ml_ev < 1e-12] <- 0
  omega_ml_ev <- omega_ml_ev + 1
  psi_ml_ev <- Re(eigen(psi_ml, only.values = TRUE)$values)
  psi_ml_ev[psi_ml_ev < 1e-12] <- 0
  psi_ml_ev <- psi_ml_ev + 1


  # Likelihood --------------------------------------------------------------

  log_mlike <- (-M * N * log(pi) / 2) +
    sum(lgamma(((N + M + 2) - 0:(M - 1)) / 2) -
          lgamma(((M + 2) - 0:(M -1)) / 2)) -
    (N * sum(log(diag(psi))) / 2) -
    (M * sum(log(omega_ml_ev)) / 2) -
    ((N + M + 2) * sum(log(psi_ml_ev)) / 2)

  # Add prior-pdfs
  if(length(priors$hyperpars)!=0){
    log_mlike <- log_mlike +
      sum(sapply(names(prior_coef), function(x) {
        log(dgamma(pars[x],
                   shape = prior_coef[[x]]$k, scale = prior_coef[[x]]$theta))
      }))
  }



  # Posteriors --------------------------------------------------------------

  if(draw) {
    S_post <- psi + sse +
      t(beta_hat - priors$b) %*% omega_inv %*% (beta_hat - priors$b)
    S_eig <- eigen(S_post)
    S_inv <- S_eig$vectors %*% diag(1 / abs(S_eig$values)) %*% t(S_eig$vectors)
    eta <- MASS::mvrnorm(n = (N + M + 2), mu = rep(0, M), Sigma = S_inv)
    sigma_draw <- solve(crossprod(eta)) %*% diag(M)
    sigma_chol <- t(chol(sigma_draw))
    beta_draw <- beta_hat +
      t(MASS::mvrnorm(n = M,
                      mu = rep(0, (1 + M * lags)),
                      Sigma = solve(crossprod(X) + omega_inv) %*%
                        diag(1 + M * lags))) %*%
      sigma_chol

    return(list("log_mlike" = log_mlike,
                "beta_draw" = beta_draw,
                "sigma_chol" = sigma_chol,
                "sigma_draw" = sigma_draw))
  } else {
    return(log_mlike)
  }
}
