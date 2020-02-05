
# Get data ----------------------------------------------------------------

# See https://research.stlouisfed.org/econ/mccracken/fred-databases/
link <- "https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/quarterly/"
file <- "2020-01.csv" # Update this

fred_qd <- read.csv(paste(link, file, sep = ""), stringsAsFactors = FALSE)

# Rows to remove
fred_trans <- fred_qd[2, -1] # Keep transformation codes
fred_qd[c(1:2, nrow(fred_qd)), ]
fred_qd <- fred_qd[-c(1:2, nrow(fred_qd)), ]

# Fill rownames with dates and remove date variable
dates <- as.Date(fred_qd$sasdate, "%m/%d/%Y")
rownames(fred_qd) <- dates
fred_qd$sasdate <- NULL

# Adjust S&P 500 names
names(fred_qd)[grep("S[.]P", names(fred_qd))]
names(fred_qd)[grep("S[.]P", names(fred_qd))] <-
  c("SP500", "SPINDUST", "SPDIVYIELD", "SPPERATIO")
names(fred_trans)[grep("S[.]P", names(fred_trans))] <-
  c("SP500", "SPINDUST", "SPDIVYIELD", "SPPERATIO")

# Test
all(vapply(fred_qd, is.numeric, logical(1)))
vapply(fred_qd, function(x) sum(is.na(x)), numeric(1))

# Save fred_qd
save(fred_qd, file = "data/fred_qd_full.rda", version = 2)
save(fred_trans, file = "data/fred_trans.rda", version = 2)


# Copyrights --------------------------------------------------------------

# Some series in the database are under copyright. We are allowed to use /
# provide most series present in the database. Here we first scrape information
# and then subset the dataset in coordination with the Federal Reserve.

load("data/fred_qd_full.rda")

# Scrape copyrights -----

# library("rvest")

# rights <- matrix(NA, nrow = ncol(fred_qd), ncol = 2)
# colnames(rights) <- c("copyright", "public_domain")

# names_url <- gsub("(.*)x", "\\1", names(fred_qd))

# for(i in seq_along(fred_qd)) {
# # for(i in which(is.na(rights[, 1] | rights[, 2]))) { # When it crashes
#   site <- paste0("https://fred.stlouisfed.org/series/", names_url[i])
#   if(RCurl::url.exists(site)) {
#     site_txt <- site %>% read_html() %>% html_text()
#     rights[i, ] <- c(grepl("copyrighted: [a-zA-Z]+ required", site_txt),
#                      grepl("public domain: citation requested", site_txt))
#   }
# }

# copyrighted <- names_url[rights[, "copyright"]]
# (copyrighted <- copyrighted[!is.na(copyrighted)])
# public_domain <- names_url[rights[, "public_domain"]]
# (public_domain <- public_domain[!is.na(public_domain)])
# not_found <- names_url[is.na(rights[, 1])]

# Filter available series -----

# According to the FRED (Adrienne Brennecke) the following series are under
# copyright:
# VXOCLS, NIKKEI225, NASDAQCOM, SP500, UMCSENT, USEPUINDXM, AAA, BAA

# We find:
# CMRMTSPL, INVCQRMTSPL, OILPRICE, MORTGAGE30US, AAA, BAA, BAA10YM, AMBSLREAL,
# M1REAL, M2REAL, MZMREAL, VXOCLS, SPCS10RSA, SPCS20RSA, TB3SMFFM, T5YFFM,
# AAAFFM, NIKKEI225, NASDAQCOM, SP500

# Update: The St. Louis Fed is the copyright owner and allows use for all but:
# OILPRICE, MORTGAGE30US, SPCS10RSA, SPCS20RSA
# (in addition to the original ones)

# Apparently UMCSENT and USEPUINDXM appear as public domain on FRED.

# Update: UMCSENT is not in public domain (error on FRED), we got permission
# for USEPUINDXM from the copyright owner (Scott Baker).

# Not-founds:
#   in public domain (source series in brackets):
# UNRATEST, UNRATELT (UNRATE)
# HWI (JTSJOL)
# AMDMNO (DGORDER)
# MORTG10YR (WGS10YR / MORTG)
# TB6M3M (DTB6 / TB3MS)
# GS1TB3M (WGS1YR / TB3MS)
# GS10TB3M (WGS10YR / TB3MS)
# CPF3MTB3M (DCPF3M / TB3MS)
# LIABPI (TLBSHNO)
# NWPI (TNWBSHNO)
# TARESA (BOGZ1FL152010055A)
# HWIURATIO (JTSJOL / UNRATE)
# CLAIMS (ICSA)
# CONSPI (NONREVSL)
# COMPAPFF (DCPF3M / FEDFUNDS)
# TLBSNNCBBDI (NCBDBIQ027S / IPDBS)
# TNWMVBSNNCBBDI (TNWMVBSNNCB)
# TLBSNNBBDI (NCBDBIQ027S)
# TNWBSNNBBDI (NNBENBA027N)
#   under copyright:
# SPINDUST, SPDIVYIELD, SPPERATIO

# Wrap up copyright:
# We get to keep all but:
remove <- c(
  # Original mail (minus "USEPUINDXM", which we got permission for)
  "VXOCLS", "NIKKEI225", "NASDAQCOM", "SP500", "UMCSENT", "AAA", "BAA",
  # Cross-check mail
  "OILPRICE", "MORTGAGE30US", "SPCS10RSA", "SPCS20RSA",
  # Not-founds, definitely under copyright
  "SPINDUST", "SPDIVYIELD", "SPPERATIO"
)
names_url <- gsub("(.*)x", "\\1", names(fred_qd))
remove %in% names_url
keep <- which(!names_url %in% remove)

# We keep the series that we have permission for (i.e. public domain or
# copyrighted with given permission):
fred_qd <- fred_qd[, keep]

save(fred_qd, file = "data/fred_qd.rda", version = 2)


# Add transformations -------------------------------------------------------

load("data/fred_qd.rda")
load("data/fred_trans.rda")

transform_fred <- function(x,
  code = c("no" = 1, "diff" = 2, "diff-2" = 3,
    "log" = 4, "log-diff" = 5, "log-diff-2" = 6, "pct-diff" = 7),
  lag = 1) {
  fun <- switch(code,
    # None
    function(x) {x},
    # First differences
    function(x) {c(rep(NA, lag), diff(x, lag = lag))},
    # Second differences
    function(x) {c(rep(NA, lag * 2), diff(diff(x, lag = lag), lag = lag))},
    # Logs
    function(x) {log(x)},
    # Log first differences
    function(x) {c(rep(NA, lag), diff(log(x), lag = lag))},
    # Log second differences
    function(x) {c(rep(NA, lag * 2), diff(diff(log(x), lag = lag), lag = lag))},
    # Percent-change differences
    function(x) {c(rep(NA, lag), x[-seq(lag)] / head(x, length(x) - lag) - 1L)}
  )
  fun(x)
}

fred_qd_trans <- vapply(names(fred_qd), function(x) {
    code <- fred_trans[grep(x, names(fred_trans))][[1]]
    if(!code %in% 1L:7L) {stop("Code for", name, "not found!")}
    transform_fred(x = fred_qd[, x], code = code, lag = 1)
  }, numeric(nrow(fred_qd)))

save(fred_qd_trans, file = "data/fred_qd_trans.rda", version = 2)
