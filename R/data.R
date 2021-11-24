
#' FRED-MD and FRED-QD: Databases for Macroeconomic Research
#'
#' FRED-MD and FRED-QD are large macroeconomic databases, containing monthly
#' and quarterly time series that are frequently used in the literature. They
#' are intended to facilitate the reproduction of empirical work and simplify
#' data related tasks.
#' Included datasets are provided as is - transformation codes are available
#' in \code{system.file("fred_trans.rds", package = "BVAR")}. These can be
#' applied automatically with \code{\link{fred_transform}}.
#'
#' The versions of FRED-MD and FRED-QD that are provided here are licensed
#' under a modified ODC-BY 1.0 license that can be found in the provided
#' \emph{LICENSE} file. The provided versions are subset to variables that are
#' either in public domain or for which we were given permission to use.
#' For further details see McCracken and Ng (2016) or
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
#' @seealso \code{\link{fred_transform}}
#'
#' @keywords FRED macroeconomics
#'
#' @references
#'   McCracken, M. W. and Ng, S. (2016) FRED-MD: A Monthly Database for
#'   Macroeconomic Research. \emph{Journal of Business & Economic Statistics},
#'   \bold{34:4}, 574-589, \doi{10.1080/07350015.2015.1086655}.
#'   McCracken, M. W., & Ng, S. (2020). FRED-QD: A Quarterly Database for
#'   Macroeconomic Research \bold{w26872}. National Bureau of Economic Research.
#'
#' @source \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
"fred_qd"

#' @rdname fred_qd
"fred_md"
