
#' FRED transformation and subset helper
#'
#' Apply transformations given by FRED-MD or FRED-QD and generate rectangular
#' subsets. See \code{\link{fred_qd}} for information on data and the Details
#' section for information on the transformations. Call without arguments to
#' retrieve available codes / all FRED suggestions.
#'
#' FRED-QD and FRED-MD include a transformation code for every variable. All
#' codes are provided in \code{system.file("fred_trans.csv", package = "BVAR")}.
#' The transformation codes are as follows:
#' \enumerate{
#'   \item \code{1} - no transformation;
#'   \item \code{2} - first differences - \eqn{\Delta x_t}{Delta x};
#'   \item \code{3} - second differences - \eqn{\Delta^2 x_t}{Delta2 x};
#'   \item \code{4} - log transformation - \eqn{\log x_t}{log x};
#'   \item \code{5} - log differences -
#'     \eqn{\Delta \log x_t}{Delta log x};
#'   \item \code{6} - log second differences -
#'     \eqn{\Delta^2 \log x_t}{Delta2 log x};
#'   \item \code{7} - percent change differences -
#'     \eqn{\Delta x_t / x_{t-1} - 1}{Delta x / lag-x - 1};
#' }
#' Note that the transformation codes of FRED-MD and FRED-QD may differ for
#' the same series.
#'
#' @param data A \code{data.frame} with FRED-QD or FRED-MD time series. The
#' column names are used to find the correct transformation.
#' @param type Character scalar. Whether \emph{data} stems from the FRED-QD or
#' the FRED-MD database.
#' @param codes Integer vector. Transformation code(s) to apply to \emph{data}.
#' Overrides automatic lookup of transformation codes.
#' @param na.rm Logical scalar. Whether to subset to rows without any
#' \code{NA} values. A warning is thrown if rows are non-sequential.
#' @param lag Integer scalar. Number of lags to apply when taking differences.
#' See \code{\link[base]{diff}}.
#' @param scale Numeric scalar. Scaling to apply to log differences.
#' @param vars Character vector. Names of the variables to look for.
#' @param table Logical scalar. Whether to return a table of matching
#' transformation codes instead of just the codes.
#'
#' @return \code{\link{fred_transform}} returns a \code{data.frame} object with
#' applied transformations. \code{\link{fred_code}} returns transformation
#' codes, or a \code{data.frame} of matching transformation codes.
#'
#' @seealso \code{\link{fred_qd}}
#'
#' @keywords datasets FRED
#'
#' @export
#'
#' @examples
#' # Transform a subset of FRED-QD
#' fred_transform(fred_qd[, c("GDPC1", "INDPRO", "FEDFUNDS")])
#'
#' # Get info on transformation codes for unemployment variables
#' fred_code("UNRATE", table = TRUE)
#'
#' # Get the transformation code for GDPC1
#' fred_code("GDPC1", type = "fred_qd")
#'
#' # Transform all of FRED-MD
#' \dontrun{
#' fred_transform(fred_md, type = "fred_md")
#' }
fred_transform <- function(
  data,
  type = c("fred_qd", "fred_md"),
  codes, na.rm = TRUE,
  lag = 1L, scale = 100) {

  if(missing(data)) {
    return(structure(1:7, names = c("none", "1st-diff", "2nd-diff", "log",
      "log-diff", "log-2nd-diff", "pct-ch-diff")))
  }

  # Data
  if(!all(vapply(data, is.numeric, logical(1))) || !is.data.frame(data)) {
    stop("Problem with the data. Please provide a numeric data.frame.")
  }
  data <- as.data.frame(data) # Deal with tibbles and close #60

  vars <- colnames(data)
  rows <- rownames(data)

  if(!missing(codes)) {
    codes <- vapply(codes, int_check, min = 1L, max = 7L,
      msg = "Invalid value for code (outside of [1, 7]).", integer(1L))
    if(length(codes) != ncol(data)) {
      stop("Please provide one transformation code per column.")
    }
  } else {
    codes <- fred_code(paste0("^", vars, "$"), type = type)
  }

  data <- vapply(seq(ncol(data)), function(i, codes, data) {
    get_transformation(codes[i], lag = lag, scale = scale)(data[, i])
  }, codes = codes, data = data, FUN.VALUE = numeric(nrow(data)))

  na_rows <- apply(data, 1, function(x) sum(is.na(x)))
  na_cols <- apply(data, 2, function(x) sum(is.na(x)))

  if(na.rm) {
    used_rows <- na_rows == 0
    if(!any(used_rows)) {
      stop("No row without NA values available. Variable ",
        vars[which.max(na_cols)], " is problematic at ", max(na_cols), " NAs.")
    }
    if(!all(na_rows[used_rows] == cummax(na_rows[used_rows]))) {
      warning("Rows used to subset are not all sequential.")
    }
    data <- data[used_rows, ]
    rows <- rows[used_rows]
  }

  data <- as.data.frame(data)
  rownames(data) <- rows
  colnames(data) <- vars

  return(data)
}


#' @rdname fred_transform
#' @export
#'
#' @importFrom utils read.table
fred_code <- function(vars, type = c("fred_qd", "fred_md"), table = FALSE) {

  fred_trans <- read.table(system.file("fred_trans.csv", package = "BVAR"),
    header = TRUE, sep = ",", na.strings = c("", "NA"))
  fred_trans[, 2:3] <- lapply(fred_trans[, 2:3], factor,
    levels = c("none", "1st-diff", "2nd-diff", "log",
      "log-diff", "log-2nd-diff", "pct-ch-diff"))

  if(missing(vars)) {
    return(fred_trans)
  }

  if(!is.character(vars) || length(vars) == 0) {
    stop("Please provide named variables to look up transformation codes.")
  }
  table <- isTRUE(table)

  if(table) {
    matches <- do.call(c, sapply(vars, grep,
      fred_trans[["variable"]], simplify = FALSE))
    if(length(matches) == 0) {message("No transformation code(s) found.")}
    return(fred_trans[matches, ])
  }

  type <- match.arg(type)

  match <- vapply(vars, function(x, y) {
    out <- grep(paste0("^", x), y)
    if(length(out) == 0) {out <- NA_integer_}
    if(length(out) > 1) {
      message("Mutiple matches for ", x, " found. Using the first one - ",
        "consider calling with `table = TRUE`.")
      out <- out[1]
    }
    return(out)
  }, y = fred_trans[["variable"]], FUN.VALUE = integer(1L))

  codes <- as.integer(fred_trans[match, type]) # No more factor

  if(any(is.na(codes))) {
    message("No transformation code(s) for ",
      paste0(vars[is.na(codes)], collapse = ", "),
      " found. Setting to 1 for no transformation.")
    codes[is.na(codes)] <- 1
  }

  return(codes)
}


#' @noRd
#'
#' @param code Integer scalar. Code of the requested transformation.
#' @param lag Integer scalar. Number of lags to apply.
#' @param scalar Numeric scalar. Scaling to apply to log differences.
#'
#' @return Returns a function that provides the requested transformation.
#'
#' @importFrom utils head
get_transformation <- function(code, lag = 1L, scale = 100) {

  code <- int_check(code, min = 0L, max = 7L, msg = "Code not found.")
  lag <- int_check(lag, min = 1L, max = Inf, msg = "Issue with provided lag.")
  scale <- num_check(scale, min = 1e-16, max = Inf, msg = "Issue with scale.")

  switch(code,
    function(x) {x}, # No transformation
    function(x) { # First differences
      c(rep(NA, lag), diff(x, lag = lag, differences = 1L))},
    function(x) { # Second differences
      c(rep(NA, lag * 2), diff(x, lag = lag, differences = 2L))},
    function(x) {log(x)}, # Logs
    function(x) { # Log first differences
      c(rep(NA, lag), diff(log(x), lag = lag, differences = 1L)) * scale},
    function(x) { # Log second differences
      c(rep(NA, lag * 2), diff(log(x), lag = lag, differences = 2L)) * scale},
    function(x) { # Percent-change differences
      c(rep(NA, lag * 2), diff(x[-seq(lag)] / head(x, length(x) - lag) - 1L,
        lag = lag, differences = 1L)) * scale}
  )
}
