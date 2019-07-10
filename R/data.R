#' FRED-QD: Quarterly Database for Macroeconomic Research
#'
#' FRED-QD is a quarterly frequency companion to FRED-MD, a large macroeconomic
#' database. It is designed to emulate the dataset used in "Disentangling the
#' Channels of the 2007-2009 Recession" by Stock and Watson (2012)
#' but also contains several additional series.
#' The included dataset is from April 2019 and contains observations from
#' 1959Q1 until 2018Q4.
#'
#' For further details see McCracken and Ng (2016) or the dataset's appendix at
#' \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}. The
#' dataset included is a subset of the full FRED-QD with the 203 of 248
#' variables that are in public domain.
#'
#' @docType data
#'
#' @format A \code{data.frame} with 240 observations of 203 (248) variables.
#'
#' @keywords datasets fred
#'
#' @references
#'   McCracken, M. W., and Ng, S. (2016). FRED-MD: A Monthly Database for Macroeconomic Research. Journal of Business & Economic Statistics, 34, 574-589. \url{https://doi.org/10.1080/07350015.2015.1086655}
#'
#'   Stock, J. H. and Watson, M. W. (2012). Disentangling the Channels of the 2007-2009 Recession. NBER Working Paper Series, 18094. \url{https://doi.org/10.3386/w18094}
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"
