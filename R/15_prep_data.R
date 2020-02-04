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


  # Allow returning betas and hyperpriors ---------------------------------

  vars_hyp <- c("ml", colnames(x[["hyper"]]))
  vars_dep <- x[["variables"]]
  vars_ind <- x[["explanatories"]]
  # Compatibility to older versions (<= 0.2.1)
  if(is.null(vars_ind)) {vars_ind <- get_expl(vars_dep, x[["meta"]][["lags"]])}

  if(is.null(vars) && is.null(vars_impulse) && is.null(vars_response)) {
    vars <- vars_hyp
  }

  choice_hyp <- vars_hyp[unique(do.call(c, lapply(vars, grep, vars_hyp)))]

  choice_dep <- if(is.null(vars_response)) {
    # If there are number-elements interpret them as independent-positions
    vars_dep[unique(c(as.integer(vars[grep("^[0-9]+$", vars)]),
      # Exclude ones matched for independents
      do.call(c, lapply(vars[!grepl("(^const|lag[0-9]+$)", vars)],
        grep, vars_dep))))]
  } else {get_var_set(vars_response, vars_dep, M = x[["meta"]][["M"]])}
  choice_dep <- choice_dep[!is.na(choice_dep)]

  choice_ind <- if(is.null(vars_impulse)) {
    # Limit to ones with "-lag#" or "constant" to separate from dependents
    vars_ind[unique(do.call(c, lapply(vars[grep("(^const|lag[0-9]+$)", vars)],
      grep, vars_ind)))]
  } else {get_var_set(vars_impulse, vars_ind, M = x[["meta"]][["K"]])}

  if(all(c(length(choice_hyp), length(choice_dep), length(choice_ind)) == 0)) {
    stop("No data fitting matching the provided vars argument found.")
  }


  # Build up required outputs ---------------------------------------------

  out <- out_vars <- out_bounds <- out_chains <- list()
  N <- x[["meta"]][["n_save"]]

  if(length(choice_hyp) > 0) { # Hyperparameters
    out[["hyper"]] <- cbind("ml" = x[["ml"]], x[["hyper"]])[seq(N), choice_hyp]
    out_vars[["hyper"]] <- choice_hyp

    out_bounds[["hyper"]] <- vapply(choice_hyp, function(z) {
      if(z == "ml") {c(NA, NA)} else {
        c(x[["priors"]][[z]][["min"]], x[["priors"]][[z]][["max"]])
      }}, double(2))

    out_chains[["hyper"]] <- lapply(chains, function(z) {
      cbind("ml" = z[["ml"]], z[["hyper"]])[seq(N), choice_hyp]
    })
  }

  if(length(choice_dep) > 0 || length(choice_ind) > 0) { # Betas
    pos_dep <- get_var_set(choice_dep,
      variables = vars_dep, M = x[["meta"]][["M"]])
    pos_ind <- get_var_set(choice_ind,
      variables = vars_ind, M = x[["meta"]][["K"]])
    K <- length(pos_dep) * length(pos_ind)

    out[["betas"]] <- grab_betas(x, N, K, pos_dep, pos_ind)
    out_vars[["betas"]] <- paste0(
      rep(vars_dep[pos_dep], length(pos_ind)), "_",
      rep(vars_ind[pos_ind], each = length(pos_dep)))

    out_bounds[["betas"]] <- matrix(NA, ncol = K, nrow = 2)

    out_chains[["betas"]] <- lapply(chains, grab_betas, N, K, pos_dep, pos_ind)
  }

  # Merge stuff and return
  out_data <- cbind(out[["hyper"]], out[["betas"]])
  out_vars <- c(out_vars[["hyper"]], out_vars[["betas"]])
  colnames(out_data) <- out_vars

  return(list(
    "data" = out_data, "vars" = out_vars,
    "chains" = c(out_chains[["hyper"]], out_chains[["beta"]]),
    "bounds" = cbind(out_bounds[["hyper"]], out_bounds[["betas"]])))
}


#' Grab draws of certain betas
#'
#' Helper function for \code{\link{prep_data}}.
#'
#' @param x A \code{bvar} object, obtained from \code{\link{bvar}}.
#' @param N,K Integer scalars. Number of rows and columns to return.
#' @param pos_dep,pos_ind Numeric vectors. Positions of desired variables.
#'
#' @return Returns a matrix with the requested data.
#'
#' @noRd
grab_betas <- function(x, N, K, pos_dep, pos_ind) {
  data <- matrix(NA, nrow = N, ncol = K)
  k <- 1
  for(i in pos_ind) {
    for(j in pos_dep) {
      data[, k] <- x[["beta"]][seq(N), i, j] # seq() for longer chains
      k <- k + 1
  }}
  return(data)
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
#' @return Returns \code{TRUE} or throws an error.
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
