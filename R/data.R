#' FRED-QD: Quarterly Database for Macroeconomic Research
#'
#' FRED-QD is a quarterly frequency companion to FRED-MD, a large macroeconomic
#' database. It is designed to emulate the dataset used in "Disentangling the
#' Channels of the 2007-2009 Recession" by \insertCite{stock2012}{BVAR},
#' but also contains several additional series.
#' The currently included dataset is from April 2019, contains observations
#' from 1959Q1 until 2018Q4, and has been subset to series that either are
#' in public domain, or we were given permission to use.
#'
#' For further details see \insertCite{mccracken2016}{BVAR} or the dataset's
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
#'  \insertAllCited{}
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"

#' @rdname fred_qd
#' @export
"fred_qd_trans"