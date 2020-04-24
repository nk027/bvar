
# Copyrights --------------------------------------------------------------

# Some series in the database are under copyright. We are allowed to use /
# provide most series present in the database. Here we first scrape information
# and then subset the dataset in coordination with the Federal Reserve.

load("data/fred_qd_full.rda")
load("data/fred_md_full.rda")

# QD ---

# # Scrape copyrights

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
#       grepl("public domain: citation requested", site_txt))
#   }
# }

# copyrighted <- names_url[rights[, "copyright"]]
# (copyrighted <- copyrighted[!is.na(copyrighted)])
# public_domain <- names_url[rights[, "public_domain"]]
# (public_domain <- public_domain[!is.na(public_domain)])
# not_found <- names_url[is.na(rights[, 1])]

# MD ---

# Ignore the ones we know from QD
fred_md_check <- fred_md[, !names(fred_md) %in% names(fred_qd)]

# Scrape copyrights

library("rvest")

rights <- matrix(NA, nrow = ncol(fred_md_check), ncol = 2)
colnames(rights) <- c("copyright", "public_domain")

names_url <- gsub("(.*)x", "\\1", names(fred_md_check))

for(i in seq_along(fred_md_check)) {
# for(i in which(is.na(rights[, 1] | rights[, 2]))) { # When it crashes
  site <- paste0("https://fred.stlouisfed.org/series/", names_url[i])
  if(RCurl::url.exists(site)) {
    site_txt <- site %>% read_html() %>% html_text()
    rights[i, ] <- c(grepl("copyrighted: [a-zA-Z]+ required", site_txt),
      grepl("public domain: citation requested", site_txt))
  }
}

copyrighted <- names_url[rights[, "copyright"]]
(copyrighted <- copyrighted[!is.na(copyrighted)])
public_domain <- names_url[rights[, "public_domain"]]
(public_domain <- public_domain[!is.na(public_domain)])
not_found <- names_url[is.na(rights[, 1])]


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

# FRED-MD Update:
#

# 2020-02-15 - We get to keep all but:
remove <- c(
  # Original mail (minus "USEPUINDXM", which we got permission for)
  "VXOCLS", "NIKKEI225", "NASDAQCOM", "SP500", "UMCSENT", "AAA", "BAA",
  # Cross-check mail
  "OILPRICE", "MORTGAGE30US", "SPCS10RSA", "SPCS20RSA",
  # Not-founds, definitely under copyright
  "SPINDUST", "SPDIVYIELD", "SPPERATIO",
  # Additions with FRED-MD
  "BAAFFM"
)
names <- union(names(fred_qd), names(fred_md))
keep <- names[which(!names %in% remove)]

writeLines(keep, "data/fred_permitted.txt")

# We keep the series that we have permission for (i.e. public domain or
# copyrighted with given permission):
# fred_qd <- fred_qd[, names(fred_qd) %in% keep]
# fred_md <- fred_md[, names(fred_md) %in% keep]

# Save the kept subset
# save(fred_qd, file = "data/fred_qd.rda", version = 2)
# save(fred_qd, file = "data/fred_md.rda", version = 2)
