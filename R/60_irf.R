bv_irf <- function(
  horizon = 12,
  fevd = FALSE,
  identification = TRUE,
  sign_restr = NULL) {

  horizon <- int_check(horizon, min = 1, max = 1e6,
                       msg = "Invalid value for horizon (outside of [1, 1e6]).")

  if(!is.logical(c(identification, fevd))){
    stop("Parameter(s) are not provided as the correct type.")
  }

  if(!is.null(sign_restr) && !all(sign_restr %in% c(-1, 0, 1))){
    stop("Sign restrictions misspecified.")
  }

  out <- list("horizon" = horizon, "fevd" = fevd,
              "identification" = identification, "sign_restr" = sign_restr)
  class(out) <- "bv_irf"

  return(out)
}


print.bv_irf <- function(x, ...) {

  cat("\nProvide object to function bvar() in order to compute impulse responses",
      "\nfor a horizon of", x$horizon, "time periods.")
  if(x$identification){
    if(is.null(x$sign_restr)){
      cat("\n\nIdentification is achieved by means of Cholesky decomposition.",
          "\nConsider ordering of variables for correct transmission of",
          "\ncontemporaneous effects between variables.")
    }else{

      signs_df <- as.data.frame(x$sign_restr)
      signs_df <- apply(signs_df, 2, factor,
                        levels = c(-1, 0, 1), labels = c("+", "0", "-"))

      cat("\n\nIdentification is achieved by means of signs restrictions.")
      cat("\nChosen restrictions:")

      paste_it <- paste0("Var", 1:nrow(signs_df), "\t")
      for(i in 1:ncol(signs_df)){
        temp_paste <- paste0(" ", signs_df[ , i], sep="\t")
        paste_it <- paste0(paste_it, temp_paste)
      }
      paste_it <- paste0(c("\nResponse of\t",
                           rep("\n\t\t", (nrow(signs_df) - 1))),
                         paste_it)
      cat("\n\t\t\tShock from",
          "\n\t\t\t",  paste0("Var", c(1:nrow(signs_df)), sep = "\t"),
          paste_it, sep = "")

      cat("\n\nColums contain the expected sign of the reaction of all",
          "\nvariables following a shock of the respective variable.")
    }
  }else{
    cat("\n\nShocks are unidentified. Note that interpretation of impulse",
        "\nresponses may be flawed.")
  }
  if(x$fevd){
    cat("\n\nForecast error variance decompositions will be computed.")
  }

}
