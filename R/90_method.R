plot.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  hyper_plot(x, ...)

}


print.bvar <- function(x, ...) {

  if(!inherits(x, "bvar")) {stop()}

  cat("\nBayesian VAR consisting of", x$meta$N, "observations,",
      x$meta$M, "variables and", x$meta$lags, "lags.")
  cat("\nHyperparameters:",
      paste(x$priors$hyper, collapse = ", "),
      "\nHyperparameter values after optimisation:",
      paste(round(x$optim$par, 3), collapse = ", "))
  cat("\nIterations (burnt / thinning): ", x$meta$n_draw, " (", x$meta$n_burn,
      "/", x$meta$n_thin, ")", sep = "")
  cat("\nAccepted draws (rate): ", x$accepted, " (",
      round(x$accepted / x$meta$n_draw, 3), ")", sep = "")

  if(!is.null(x$irf)) {print(x$irf)}
  if(!is.null(x$fcast)) {print(x$fcast)}

  return(invisible(x))
}


print.bvar_irf <- function(x, ...) {

  if(!inherits(x, "bvar_irf")) {stop()}

  id <- x$setup$identification
  sign_restr <- is.null(x$setup$sign_restr)
  fevd <- is.null(x$fevd)

  cat("\nImpulse responses with a horizon of ", x$setup$horizon,
      " time periods.", sep = "")
  cat("\nIdentification", if(id) {"achieved by means of"
      if(sign_restr) {"Choleski decomposition."} else {"sign restrictions."}
    } else {"has been skipped."})

  # Maybe explanation how to handle object if someone wants computations?
  # cat("\nIn order to compute a function (FUN) of the responses use",
  #     "\napply(x[['irf']][['irf']], c(2, 3, 4), FUN, ...).")

  if(!fevd) {cat("\n\nForecast error variance decompositions included.")}

  # Maybe explanation how to handle object if someone wants computations?
  # cat("\nIn order to compute a function (FUN) of the responses use",
  #     "\napply(x[['irf']][['fevd']], c(2, 3), FUN, ...).")

  return(invisible(x))
}


print.bvar_fcast <- function(x, ...) {

  if(!inherits(x, "bvar_fcast")) {stop()}

  cat("\nForecast with a horizon of ", x$setup$horizon, " time periods.",
      sep = "")
  # Maybe explanation how to handle object if someone wants computations?
  # cat("\nIn order to compute a function (FUN) of the responses use",
  #     "\napply(x[['fcast']], c(2, 3), FUN, ...).")

  return(invisible(x))
}


print.bv_irf <- function(x, ...) {

  if(!inherits(x, "bv_irf")) {stop()}

  cat("Object with settings for computing impulse responses to `bvar()`.")

  if(x$identification) {
    if(is.null(x$sign_restr)) {
      cat("\n\nIdentification is achieved by means of Cholesky decomposition.",
          "Consider ordering the variables for correct transmission of",
          "contemporaneous effects.")
    } else {
      sign_restr <- apply(x$sign_restr, 2, factor,
                          levels = c(-1, 0, 1), labels = c("-", "0", "+"))
      cat("\n\nIdentification is achieved by means of sign restrictions.")
      cat("\nChosen restrictions:")

      if(length(sign_restr) > 9 ^ 2) {
        cat("\n\t\t\tShock to\n\t\t\t",
            paste0("Var", 1:nrow(sign_restr), sep = "\t"),
            paste0(c("\nResponse of\t", rep("\n\t\t", nrow(sign_restr) - 1)),
                   "Var", 1:nrow(sign_restr), "\t",
                   apply(sign_restr, 1, function(x)
                     paste0(" ", x, sep = "\t", collapse = "")),
                   collapse = "\n"))
      } else if(length(sign_restr) < 18 ^ 2) {
        print(sign_restr)
      }

      # paste_it <- paste0("Var", 1:nrow(sign_restr), "\t")
      # for(i in 1:ncol(sign_restr)) {
      #   temp_paste <- paste0(sign_restr[, i], sep = "\t")
      #   paste_it <- paste0(paste_it, temp_paste)
      # }
      # paste_it <- paste0(c("\nResponse of\t",
      #                      rep("\n\t\t", nrow(sign_restr) - 1)),
      #                    paste_it)
      # cat("\n\t\t\tShock to",
      #     "\n\t\t\t", paste0("Var", c(1:nrow(sign_restr)), sep = "\t"),
      #     paste_it, sep = "")

      cat("\n\nColums contain the expected sign of the reaction of all",
          "variables following a shock of the respective variable.")
    }
  } else {
    cat("\n\nShocks are unidentified. Note that interpretation of impulse",
        "responses may not be possible.")
  }
  if(x$fevd){
    cat("\n\nForecast error variance decompositions will be computed.")
  }

  return(invisible(x))
}


print.bv_fcast <- function(x, ...) {

  if(!inherits(x, "bv_fcast")) {stop()}

  if(is.null(x$conditional)) {
    cat("\nProvide object to function bvar() as option fcast in order to compute",
        "\nunconditional forecasts for a horizon of", x$horizon, "time periods.")
  } else {
    # Add something about conditional forecasts here once implemented
    # Path given etc.
  }

  return(invisible(x))
}
