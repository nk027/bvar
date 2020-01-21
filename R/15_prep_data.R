#' Prepare BVAR data for plotting etc.
#'
#' Helper function to retrieve hyperparameters or coefficient values based on
#' name / position. Also supports multiple \code{bvar} objects and may be used
#' to check them for similarity.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param vars Optional character vector used to subset the plot. The elements
#' need to match the names of hyperparameters (plus \code{"ml"}). Defaults
#' to \code{NULL}, i.e. all variables.
#' @param vars_response,vars_impulse Optional integer vectors with the
#' positions of coefficient values to retrieve densities of.
#' \emph{vars_response} corresponds to a specific dependent variable,
#' \emph{vars_impulse} to an independent one. Note that the constant is found
#' at position one.
#' @param chains List with additional \code{bvar} objects. Contents are then
#' added to trace and density plots.
#' @param check_chains Logical scalar. Whether to check \emph{x} and
#' \emph{chains} for similarity.
#' @param ... Fed to \code{\link{chains_fit}}.
#'
#' @return Returns a named list with:
#' \itemize{
#'   \item \code{data} - Numeric matrix with desired data.
#'   \item \code{vars} - Character vector with names for the desired data.
#'   \item \code{chains} - List of numeric matrices with desired data.
#'   \item \code{bounds} - Numeric matrix with optional boundaries.
#' }
#'
#' @noRd
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

  # Check whether to return betas or hyperparameters
  vars_hyp <- c("ml", colnames(x[["hyper"]]))
  vars_dep <- x[["variables"]]
  vars_ind <- x[["explanatories"]]

  pos_hyp <- unique(do.call(c, lapply(vars, grep, vars_hyp)))
  pos_dep <- unique(do.call(c, lapply(vars, grep, vars_dep)))
  pos_ind <- unique(do.call(c, # Limit to ones with "-lag#" to separate
    lapply(vars[grep("-lag[0-9]+$", vars)], grep, vars_ind)))

  # To-do: Allow for both, also make `vars` work for hypers & betas


  # Betas -----------------------------------------------------------------

  if(!is.null(vars_response) || !is.null(vars_impulse)) {

    vars_response <- get_var_set(vars_response,
      variables = get_expl(x[["variables"]], x[["meta"]][["lags"]]),
      M = x[["meta"]][["K"]])
    vars_impulse <- get_var_set(vars_impulse,
      variables = x[["variables"]], M = x[["meta"]][["M"]])

    grab_data <- function(z, n_row, n_col, vars_response, vars_impulse) {
      data <- matrix(NA, nrow = n_row, ncol = n_col)
      k <- 1
      for(i in seq_along(vars_response)) {for(j in seq_along(vars_impulse)) {
        data[, k] <- z[["beta"]][seq(n_row), i, j] # seq() for longer chains
        k <- k + 1
      }}
      return(data)
    }

    n_col <- length(vars_response) * length(vars_impulse)
    n_row <- x[["meta"]][["n_save"]]

    data <- grab_data(x, n_row, n_col, vars_response, vars_impulse)

    vars <- paste0("dep", vars_response, "-ind", vars_impulse)
    bounds <- matrix(0, ncol = length(vars))

    chains <- lapply(chains, grab_data,
      n_row, n_col, vars_response, vars_impulse)


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

    chains <- lapply(chains, function(z) {
      cbind("ml" = z[["ml"]], z[["hyper"]])[, vars]
    })

  }


  return(list(
    "data" = as.matrix(data),
    "vars" = vars,
    "chains" = lapply(chains, as.matrix),
    "bounds" = bounds
  ))
}


#' Check equalities across chains
#'
#' Function to help check whether \code{bvar} objects are close enough to
#' compare. Accessed via \code{\link{prep_data}}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param chains List with additional \code{bvar} objects.
#' @param Ms Logical scalar. Whether to check equality of
#' \code{x[["meta"]][["M"]]}.
#' @param n_saves Logical scalar. Whether to check equality of
#' \code{x[["meta"]][["n_save"]]}.
#' @param hypers Logical scalar. Whether to check equality of
#' \code{x[["priors"]][["hyper"]]}.
#'
#' @noRd
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
