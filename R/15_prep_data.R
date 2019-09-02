prep_data <- function(
  x,
  vars = NULL,
  vars_response = NULL, vars_impulse = NULL,
  chains = list(),
  check_chains = FALSE, ...) {

  if(!inherits(x, "bvar")) {stop("Please provide a `bvar` object.")}

  if(inherits(chains, "bvar")) {chains <- list(chains)}
  lapply(chains, function(x) {if(!inherits(x, "bvar")) {
    stop("Please provide `bvar` objects to the chains parameter.")
  }})

  # Check whether all of the chains fit together
  if(check_chains) {chains_fit(x, chains, ...)}


  # Betas -----------------------------------------------------------------

  if(!is.null(vars_response) || !is.null(vars_impulse)) {

    vars_response <- get_var_set(vars_response, M = x[["meta"]][["M"]])
    vars_impulse <- get_var_set(vars_impulse, M = x[["meta"]][["K"]])

    n_col <- length(vars_response) * length(vars_impulse)
    n_row <- x[["meta"]][["n_save"]]
    beta <- x[["beta"]]

    data <- matrix(NA, nrow = n_row, ncol = n_col)
    k <- 1
    for(i in seq_along(vars_response)) {for(j in seq_along(vars_impulse)) {
      data[, k] <- beta[, j, i]
      k <- k + 1
    }}
    vars <- paste0("dep", vars_response, "-ind", vars_impulse)
    bounds <- matrix(0, ncol = length(vars))

    chains <- lapply(chains, function(x) {
      data <- matrix(NA, nrow = n_row, ncol = n_col)
      k <- 1
      for(i in seq_along(vars_response)) {for(j in seq_along(vars_impulse)) {
        data[, k] <- beta[, j, i]
        k <- k + 1
      }}
      data
    })


  # Hyperparameters -------------------------------------------------------

  } else {

    data <- cbind("ml" = x[["ml"]], x[["hyper"]]) # Here we subset later

    if(is.null(vars)) {
      vars <- colnames(data)
    } else if(!all(vars %in% colnames(data))) {
      stop("Parameter named '",
           paste0(vars[!vars %in% colnames(data)], collapse = ", "),
           "' not found.")
    }

    bounds <- vapply(vars, function(z) {
      if(z == "ml") {c(NA, NA)} else {
        c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])
      }}, double(2))
    data <- data[, vars]

    chains <- lapply(chains, function(x) {
      cbind("ml" = x[["ml"]], x[["hyper"]])[, vars]
    })

  }


  return(list(
    "data" = as.matrix(data),
    "vars" = vars,
    "chains" = lapply(chains, as.matrix),
    "bounds" = bounds
  ))
}


chains_fit <- function(
  x, chains,
  Ms = TRUE,
  n_saves = FALSE,
  hypers = FALSE) {

  if(is.null(chains) || length(chains) == 0) {return(TRUE)}

  if(Ms) {
    Ms <- c(x[["meta"]][["M"]],
            vapply(chains, function(x) {x[["meta"]][["M"]]},
                   integer(1)))
    if(!all(duplicated(Ms)[-1])) {stop("Variables do not match.")}
  }
  if(n_saves) {
    n_saves <- c(x[["meta"]][["n_save"]],
                 vapply(chains, function(x) {x[["meta"]][["n_save"]]},
                        integer(1)))
    if(!all(duplicated(n_saves)[-1])) {stop("Saved iterations do not match.")}
  }
  if(hypers) {
    hypers <- vapply(chains, function(z) {
      x[["priors"]][["hyper"]] == z[["priors"]][["hyper"]]
    }, logical(1))
    if(!all(hypers)) {stop("Hyperparameters do not match.")}
  }

  return(TRUE)
}
