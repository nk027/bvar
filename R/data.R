#' FRED-MD and FRED-QD: Databases for Macroeconomic Research
#'
#' FRED-MD and FRED-QD are large macroeconomic databases. They contain monthly
#' and quarterly time series that are frequently used in the literature. The
#' datasets are updated in real-time through the FRED database. They are
#' intended to facilitate the reproduction of empirical work and simplify
#' data related tasks.
#' The included datasets are provided as is - transformation codes are provided
#' in \code{system.file("fred_trans.rds", package = "BVAR")}. These can be
#' applied automatically with \code{\link{fred_transform}}.
#'
#' The versions of FRED-MD and FRED-QD that are provided here are licensed
#' under a modified ODC-BY 1.0 license that can be found in the provided
#' \emph{LICENSE} file. The provided versions are subset to 121 (of 128) and
#' 237 (of 248) variables that are either in public domain or for which we were
#' given permission to use. For further details see McCracken and Ng (2016) or
#' \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}.
#' We would like to thank Michael McCracken and Serena Ng, Adrienne Brennecke
#' and the Federal Reserve Bank of St. Louis for creating, updating and making
#' available the datasets and many of the contained time series. We also thank
#' all other owners of included time series that permitted their use.
#'
#' @docType data
#'
#' @format A \code{data.frame} object with dates as rownames.
#'
#' @keywords dataset FRED macroeconomics
#'
#' @references
#'   McCracken, M. W. and Ng, S. (2016) FRED-MD: A Monthly Database for
#'   Macroeconomic Research. \emph{Journal of Business & Economic Statistics},
#'   \bold{34:4}, 574-589, \url{https://doi.org/10.1080/07350015.2015.1086655}.
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"

#' @rdname fred_qd
"fred_md"


#' FRED transformation and subsetting helper
#'
#' Apply transformations given by FRED-MD or FRED-QD and generate rectangular
#' subsets. See \code{\link{fred_qd}} for information on data and the details
#' section for information on the transformations.
#'
#' FRED-QD and FRED-MD include a transformation code for every variable. All
#' codes are provided in \code{system.file("fred_trans.rds", package = "BVAR")}.
#' The transformation codes are as follows:
#' \enumerate{
#'   \item \code{1} - no transformation;
#'   \item \code{2} - first differences - \eqn{\delta x_t};
#'   \item \code{3} - second differences - \eqn{\delta^2 x_t};
#'   \item \code{4} - log transformation - \eqn{log(x_t)};
#'   \item \code{5} - log differences - \eqn{\delta^2 log(x_t)};
#'   \item \code{6} - percent change differences -
#'   \eqn{\delta x_t / x_{t-1} - 1};
#' }
#' Note that the transformation codes of FRED-MD and FRED-QD may differ for
#' the same series.
#'
#' @param data A \code{data.frame} with FRED-QD or FRED-MD time series. The
#' column names are used to find the correct transformation.
#' @param type Character scalar. Specify whether \emph{data} stems from the
#' FRED-QD or FRED-MD database.
#' @param transform Logical scalar. Whether to apply transformations.
#' @param na.rm Logical scalar. Whether to subset to rows without any
#' \code{NA} values. A warning is thrown if rows are non-sequential.
#' @param lag Integer scalar. Number of lags to apply in the transformation
#' when taking differences. Defaults to 1.
#' @param scale Numeric scalar. Factor by which to scale up values after taking
#' differences. Defaults to 100.
#'
#' @return Returns a \code{data.frame} object.
#'
#' @seealso \code{\link{fred_qd}};
#'
#' @export
#'
#' @examples
#' # Transform a subset of FRED-QD
#' fred_transform(fred_qd[, c("GDPC1", "INDPRO", "FEDFUNDS")])
#'
#' # Transform all of FRED-MD and subset to a rectangular shape
#' \dontrun{
#' fred_transform(fred_md, type = "md")
#' }
fred_transform <- function(
  data,
  type = c("qd", "md"),
  transform = TRUE, na.rm = TRUE,
  lag = 1, scale = 100) {

  # Data
  if(!all(vapply(data, is.numeric, logical(1))) || !is.data.frame(data)) {
    stop("Problem with the data. Please provide a numeric data.frame.")
  }

  vars <- colnames(data)
  rows <- rownames(data)

  if(transform) {
    codes <- lookup_code(vars, type = type)
    data <- vapply(seq(ncol(data)), function(i, codes, data) {
      get_transformation(codes[i], lag = lag, scale = scale)(data[, i])
    }, codes = codes, data = data, FUN.VALUE = numeric(nrow(data)))
  }

  na_rows <- apply(data, 1, function(x) sum(is.na(x)))
  na_cols <- apply(data, 2, function(x) sum(is.na(x)))
  used_rows <- na_rows == 0

  if(na.rm) {
    if(!any(used_rows)) {
      stop("No row without NA values available. Variable ",
        vars[which.max(na_cols)], " is problematic at ", max(na_cols), " NAs.")
    }
    if(!all(na_rows[used_rows] == cummax(na_rows[used_rows]))) {
      warning("Rows used to subset are not all sequential.")
    }
    data <- data[used_rows, ]
  }
  data <- as.data.frame(data)
  rownames(data) <- rows[na_rows == 0]
  colnames(data) <- vars

  return(data)
}


#' @noRd
lookup_code <- function(vars, type = c("qd", "md")) {

  if(!is.character(vars) || length(vars) == 0) {
    stop("Please provide a character vector to look up transformation codes.")
  }
  type <- match.arg(type)

  fred_trans <- readRDS(system.file("fred_trans.rds", package = "BVAR"))
  match <- vapply(vars, function(x, y) {
    out <- grep(paste0("^", x, "$"), y)
    if(length(out) == 0) {out <- NA_integer_}
    return(out)
  }, y = fred_trans$variable, FUN.VALUE = integer(1L))
  code <- fred_trans[match, type]

  if(any(is.na(code))) {
    message("No transformation code for ",
      paste0(vars[is.na(code)], collapse = ", "),
      " found. Setting to 1 for no transformation.")
    code[is.na(code)] <- 1
  }

  return(code)
}


#' @noRd
get_transformation <- function(code, lag = 1L, scale = 100L) {

  code <- int_check(code, min = 0L, max = 7L, msg = "Code not found.")
  lag <- int_check(lag, min = 1L, max = Inf, msg = "Issue with provided lag.")
  scale <- num_check(scale, min = 1e-16, max = Inf,
    msg = "Issue with provided scale.")

  switch(code,
    function(x) {x}, # No transformation
    function(x) { # First differences
      c(rep(NA, lag), diff(x, lag = lag, differences = 1)) * scale},
    function(x) { # Second differences
      c(rep(NA, lag * 2), diff(x, lag = lag, differences = 2)) * scale},
    function(x) {log(x)}, # Logs
    function(x) { # Log first differences
      c(rep(NA, lag), diff(log(x), lag = lag, differences = 1)) * scale},
    function(x) { # Log second differences
      c(rep(NA, lag * 2), diff(log(x), lag = lag, differences = 2)) * scale},
    function(x) { # Percent-change differences
      c(rep(NA, lag), x[-seq(lag)] / head(x, length(x) - lag) - 1L) * scale}
  )
}
