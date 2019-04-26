
# See https://research.stlouisfed.org/econ/mccracken/fred-databases/
link <- "https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/quarterly/"
file <- "2019-03.csv"

fred_qd <- read.csv(paste(link, file, sep = ""), stringsAsFactors = FALSE)

# Rows to remove
fred_qd[c(1:2, nrow(fred_qd)), ]
fred_qd <- fred_qd[-c(1:2, nrow(fred_qd)), ]

# Fill rownames with dates and remove date variable
dates <- as.Date(fred_qd[[1]], "%m/%d/%Y")
rownames(fred_qd) <- dates
fred_qd$sasdate <- NULL

# Adjust S&P 500 names
names(fred_qd)[grep("S[.]P", names(fred_qd))]
names(fred_qd)[grep("S[.]P", names(fred_qd))] <-
  c("SP500", "SPINDUST", "SPDIVYIELD", "SPPERATIO")

# Test
all(vapply(fred_qd, is.numeric, logical(1)))
vapply(fred_qd, function(x) sum(is.na(x)), numeric(1))

# Save fred_qd
save(fred_qd, file = "data/fred_qd.rda")
