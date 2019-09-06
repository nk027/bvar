
# Get data ----------------------------------------------------------------

# See https://research.stlouisfed.org/econ/mccracken/fred-databases/
link <- "https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/quarterly/"
file <- "2019-03.csv" # Update this

fred_qd <- read.csv(paste(link, file, sep = ""), stringsAsFactors = FALSE)

# Rows to remove
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

# Test
all(vapply(fred_qd, is.numeric, logical(1)))
vapply(fred_qd, function(x) sum(is.na(x)), numeric(1))

# Save fred_qd
save(fred_qd, file = "data/fred_qd_full.rda", version = 2)


# Get copyright info ------------------------------------------------------

# Some series in the database are under copyright. We are currently waiting
# for the Federal Reserve to provide us with information on allowed usage. In
# the meantime any questionable series are removed.

library(rvest)

load("data/fred_qd_full.rda")

rights <- matrix(NA, nrow = ncol(fred_qd), ncol = 2)
colnames(rights) <- c("copyright", "public_domain")

names_url <- gsub("(.*)x", "\\1", names(fred_qd))

for(i in seq_along(fred_qd)) {
# for(i in which(is.na(rights[, 1] | rights[, 2]))) { # When it crashes

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

# We find:
# CMRMTSPL, INVCQRMTSPL, OILPRICE, MORTGAGE30US, AAA, BAA, BAA10YM, AMBSLREAL,
# M1REAL, M2REAL, MZMREAL, VXOCLS, SPCS10RSA, SPCS20RSA, TB3SMFFM, T5YFFM,
# AAAFFM, NIKKEI225, NASDAQCOM, SP500

# Apparently UMCSENT and USEPUINDXM are put in public domain by their owners

# Not-founds in public domain (source series in brackets):
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

# Not-founds under copyright:
# SPINDUST, SPDIVYIELD, SPPERATIO

copyrighted <- names_url[rights[, "copyright"]]
(copyrighted <- copyrighted[!is.na(copyrighted)])
public_domain <- names_url[rights[, "public_domain"]]
(public_domain <- public_domain[!is.na(public_domain)])
not_found <- names_url[is.na(rights[, 1])]

public_domain <- c(public_domain,
                   not_found[!not_found %in%
                               c("SPINDUST", "SPDIVYIELD", "SPPERATIO")])

# We keep the series that are in public domain:
fred_qd <- fred_qd[, which(rights[, "public_domain"])]

save(fred_qd, file = "data/fred_qd.rda", version = 2)
