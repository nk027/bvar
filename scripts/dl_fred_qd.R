

# Get data ----------------------------------------------------------------

# See https://research.stlouisfed.org/econ/mccracken/fred-databases/
link <- "https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/quarterly/"
file <- "2019-03.csv"

fred_qd <- base::read.csv(paste(link, file, sep = ""), stringsAsFactors = FALSE)

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


# Get copyright info ------------------------------------------------------

# Some series in the database are under copyright. We are currently waiting
# for the Federal Reserve to provide us with information on allowed usage. In
# the meantime any questionable series are removed.

library(rvest)

data("fred_qd")

rights <- matrix(NA, nrow = ncol(fred_qd), ncol = 2)
colnames(rights) <- c("copyright", "public_domain")

names_url <- gsub("(.*)x", "\\1", names(fred_qd))

for(i in seq_along(fred_qd)) {

  site <- paste0("https://fred.stlouisfed.org/series/", names_url[i])

  if(RCurl::url.exists(site)) {
    site_txt <- site %>%
      read_html() %>%
      html_text()

    rights[i, ] <- c(grepl("copyrighted: [a-zA-Z]+ required", site_txt),
                     grepl("public domain: citation requested", site_txt))
  }
}

# According to FRED (Adrienne Brennecke) the following series are under copyright:
# VXOCLS, NIKKEI225, NASDAQCOM, SP500, UMCSENT, USEPUINDXM, AAA, BAA

# Here we keep the ones explicitly in public domain.
fred_qd <- fred_qd[, which(rights[, "public_domain"])]

save(fred_qd, file = "data/fred_qd.rda")
