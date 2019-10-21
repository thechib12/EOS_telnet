# Loading of csv file
scans <- read.csv("~/Documents/Uni/UT/_EoS/_assignments/scan/telnetscan.csv", stringsAsFactors = FALSE)
results <- read.csv("~/Documents/Uni/UT/_EoS/_assignments/scan/results.csv", stringsAsFactors = FALSE)
scans_b <- scans
scans_u <- subset(scans_b, !duplicated(ip)) 
scans <- scans_b
scans <- scans[!(scans$hostname == ""), ]

# List of scanned IPs
scans_u <- unique(scans[,2])

# List of disclosing IPs
scans_disc <- dplyr::filter(scans, grepl("v[0-9]\\.[0-9]|Telnet|SSH|MikroTik|RICOH|Huawei|FTP|SMTP|Linux|Microsoft|Cisco|Broadband|NNTP|NNRP|Debian|Raspbian|OpenSSH|openpli|DiskStation|Router|ZyXEL|SRX[0-9]+|VMG[0-9]+|NBG[0-9]+|FVS[0-9]+|RT[0-9]+|ZEM[0-9]+|UR5", banner))
scans_disc_u <- unique(scans_disc[,2])

# Add a column to the result dataframe
results_new <- data.frame(results)
results_new$disclosing <- ""

dates <- seq(as.Date("2018-01-01"), as.Date("2018-09-01"), by=1)

for(i in 1:nrow(results_new)){
  # Create temp vector for fractions
  tmp <- vector()
  # Drop everithing that doesn't have this name
  scans_tmp <- dplyr::filter(scans, grepl(results_new$X[i], hostname))
  # Per day
  for (d in  seq_along(dates)){
    # Get the daily scans
    ip_d <- dplyr::filter(scans_tmp, grepl(dates[[d]], timestamp))
    # Get the number of unique IPs 
    ips <- unique(ip_d[,2])
    # See how many are in the disclosing vector
    disc <- intersect(ips, scans_disc_u)
    # Calculate the fraction
    frac <- length(disc)/length(ips)
    # Add to vector
    tmp <- c(tmp, frac)
  }
  # Remove empty days
  tmp <- na.omit(tmp)
  # Mean of daily percentages
  results_new$disclosing[i] <- mean(tmp)
  # Progress status
  print(i/nrow(results_new))
}

write.csv(results_new, "results.csv", row.names = F)
