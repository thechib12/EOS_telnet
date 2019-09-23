#---> MikroTik Vulnerability Analysis CVE-2018-7445 10 <---

# Loading csv file
scans <- read.csv("~/Downloads/telnetscan.csv")
# Select all the entries mentioning MikroTik
mt <- dplyr::filter(scans, grepl('MikroTik', banner))
# Number of unique IPs running MikroTik
mt_unique <- unique(mt[,2])


## Unique IPs running MikroTik OS between the patch release and the vulnerability disclosure
mt_patchDisc <- data.frame(mt)
# Discard entries until the patch release
dates <- seq(as.Date("2018-01-01"), as.Date("2018-03-11"), by=1)
for (d in  seq_along(dates)){
  mt_patchDisc <- dplyr::filter(mt_patchDisc, !grepl(dates[[d]], timestamp))
}
# Discard entries from the vulnerability disclosure
dates <- seq(as.Date("2018-03-19"), as.Date("2018-09-01"), by=1)
for (d in  seq_along(dates)){
  mt_patchDisc <- dplyr::filter(mt_patchDisc, !grepl(dates[[d]], timestamp))
}
mt_patchDisc_unique <- unique(mt_patchDisc[,2])
# Non vulnerable IPs
mt_patchDisc_safe <- dplyr::filter(mt_patchDisc, grepl('v6.41.3|v6.41.4|v6.42', banner))
mt_patchDisc_safe_unique <- unique(mt_patchDisc_safe[,2])



## Unique IPs running MikroTik OS between the vulnerability disclosure and March 31st
mt_discApril <- data.frame(mt)
# Discard entries before the vulnerability disclosure
dates <- seq(as.Date("2018-01-01"), as.Date("2018-03-18"), by=1)
for (d in  seq_along(dates)){
  mt_discApril <- dplyr::filter(mt_discApril, !grepl(dates[[d]], timestamp))
}
# Discard entries after March
dates <- seq(as.Date("2018-04-01"), as.Date("2018-09-01"), by=1)
for (d in  seq_along(dates)){
  mt_discApril <- dplyr::filter(mt_discApril, !grepl(dates[[d]], timestamp))
}
mt_discApril_unique <- unique(mt_discApril[,2])
# Non vulnerable IPs
mt_discApril_safe <- dplyr::filter(mt_discApril, grepl('v6.41.3|v6.41.4|v6.42', banner))
mt_discApril_safe_unique <- unique(mt_discApril_safe[,2])



## Unique IPs running MikroTik OS from the patch release to the end of June
mt_after <- data.frame(mt)
# Discard entries until the patch release
dates <- seq(as.Date("2018-01-01"), as.Date("2018-03-11"), by=1)
for (d in  seq_along(dates)){
  mt_after <- dplyr::filter(mt_after, !grepl(dates[[d]], timestamp))
}
mt_after_unique <- unique(mt_after[,2])
# Non vulnerable IPs
mt_after_safe <- dplyr::filter(mt_after, grepl('v6.41.3|v6.41.4|v6.42', banner))
mt_after_safe_unique <- unique(mt_after_safe[,2])



# MT devices scanned in both intervals
mt_all_intervals <- intersect(mt_patchDisc_unique,mt_discApril_unique)
# Mt non vulnerable devices scanned in both intervals
mt_patchDisc_safe_unique <- intersect(mt_patchDisc_safe_unique,mt_all_intervals)
mt_discApril_safe_unique <- intersect(mt_discApril_safe_unique,mt_all_intervals)
