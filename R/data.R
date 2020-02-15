#' FRED-MD and FRED-QD: Databases for Macroeconomic Research
#'
#' FRED-MD and FRED-QD are large macroeconomic databases. They contain monthly
#' and quarterly time series that are frequently used in the literature. The
#' datasets are updated in real-time through the FRED database. They are
#' intended to facilitate the reproduction of empirical work and simplify
#' data related tasks.
#' The included datasets are provided as is, and in a pre-transformed version.
#' See \code{\link{fred_transform}} for the applied transformations and the
#' details section for further information on FRED-MD and FRED-QD.
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
#'   Michael W. McCracken and Serena Ng (2016). FRED-MD: A Monthly Database for
#'   Macroeconomic Research. Journal of Business & Economic Statistics, 34:4,
#'   574-589, DOI: 10.1080/07350015.2015.1086655.
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"

#' @rdname fred_qd
"fred_md"

#' @rdname fred_qd
"fred_qd_trans"

#' @rdname fred_qd
"fred_md_trans"


#' FRED transformation helper
#'
#' Retrieve and apply transformations from FRED-MD or FRED-QD. See
#' \code{\link{fred_qd}} for information on data and the details section for
#' information on available transformations. Note that the transformation
#' codes of FRED-MD and FRED-QD may differ.
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
#' @param type Character scalar. Specify whether to return both FRED-QD and
#' FRED-MD transformation codes (\code{"full"}) or subset to one (via
#' \code{"qd"} / \code{"md"}).
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
#' @importFrom utils head
#'
#' @examples
fred_transform <- function(vars, type = c("full", "qd", "md"),
  code, lag = 1, scale = 100) {

  if(missing(vars) && !missing(code)) {
    return(get_transformation(code, lag, scale))
  }

  if(!is.character(vars) || length(vars) == 0) {
    stop("Please provide a character vector to look up transformation codes.")
  }
  type <- match.arg(type)
  system.file("fred_trans.rda", package = "BVAR")
  code <- fred_trans[do.call(c, lapply(vars, grep, fred_trans$variable)), ]
  if(nrow(code) == 0) {stop("Variable not found.")}
  if(type != "full") {code <- code[, type]}

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
