#' FRED-QD: Quarterly Database for Macroeconomic Research
#'
#' FRED-QD is a quarterly frequency companion to FRED-MD, a large macroeconomic
#' database. It is designed to emulate the dataset used in "Disentangling the
#' Channels of the 2007-2009 Recession" by Stock and Watson (2012),
#' but also contains several additional series.
#' The currently included dataset is from April 2019, contains observations
#' from 1959Q1 until 2018Q4, and has been subset to series that either are
#' in public domain, or we were given permission to use.
#'
#' For further details see McCracken and Ng (2016) or the dataset's
#' appendix at
#' \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}.
#' The FRED-QD database is made available under a modified ODC-BY 1.0 license,
#' which can be found in the provided \emph{LICENSE} file.
#' Thanks to Michael McCracken, Adrienne Brennecke and the Federal Reserve
#' Bank of St. Louis for making this database available and their
#' responsiveness and help regarding licensing issues.
#'
#' @docType data
#'
#' @format A \code{data.frame} with 240 observations of 234 (248) variables.
#'
#' @keywords dataset FRED macroeconomics
#'
#' @references
#'  ToDo
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"

#' @rdname fred_qd
"fred_qd_trans"




#' FRED transformation helper
#'
#' Retrieve and apply transformations from FRED-MD or FRED-QD. See
#' \code{\link{fred_md}} and \code{\link{fred_qd}} for information on data and
#' the details section for information on available transformations.
#'
#' The following transformation codes are available:
#' \enumerate{
#'   \item \code{1} - no transformation;
#'   \item \code{2} - first differences - \eqn{\delta x_t};
#'   \item \code{3} - second differences - \eqn{\delta^2 x_t};
#'   \item \code{4} - log transformation - \eqn{log(x_t)};
#'   \item \code{5} - log differences - \eqn{\delta^2 log(x_t)};
#'   \item \code{6} - percent change differences -
#'   \eqn{\delta x_t / x_{t-1} - 1};
#' }
#'
#' @param vars Character vector.
#' @param code Integer scalar.
#' @param lag Integer scalar. Number of lags to apply in the returned function
#' when taking differences. Defaults to 0.
#' @param scale Numeric scalar. Factor by which to scale up values after taking
#' differences in the returned function. Defaults to 100.
#'
#' @return Given \emph{vars} returns a named vector of transformation codes
#' for matching variables. Given \emph{code} returns a function to perform the
#' transformation. Note that this function does not perform input checks.
#'
#' @seealso \code{\link{fred_md}}; \code{\link{fred_qd}};
#'
#' @export
#'
#' @examples
#'
#'
fred_transform <- function(vars, code, lag = 1, scale = 100) {

  if(missing(vars) && !missing(code)) {
    return(get_transformation(code, lag, scale))
  }

  if(!is.character(vars) || length(vars) == 0) {
    stop("Please provide a character vector to look up transformation codes.")
  }

  system.file("fred_trans.rda", package = "BVAR")
  code <- fred_trans[do.call(c, lapply(vars, grep, names(fred_trans)))]
  if(length(code) == 0) {stop("Variable not found.")}

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
