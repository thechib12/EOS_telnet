#---> Find IPs disclosing information on running software <---

# Loading of csv file
scans <- read.csv("~/Documents/Uni/UT/_EoS/_assignments/scan/telnetscan.csv")
# Isolating messages giving information about running software/services
sw_version <- dplyr::filter(scans, grepl("RICOH|Huawei|FTP|SMTP|Linux|Microsoft|Cisco|Broadband|NNTP|NNRP|Debian|Raspbian|OpenSSH|openpli|DiskStation|Router|ZyXEL|SRX[0-9]+|VMG[0-9]+|NBG[0-9]+|FVS[0-9]+|RT[0-9]+|ZEM[0-9]+|UR5", banner))



# PORT23 #######################################################################################
# Scans on port 23
scans_23 <- subset(scans, port == 23)
# Messages disclosing information on port 23
sw_version_23 <- subset(sw_version, port == 23)

# Total number of unique IPs scanned on port 23
print(length(unique(scans_23[,2])))
# Total number of unique IPs disclosing information on port 23
print(length(unique(sw_version_23[,2])))
# Fraction of disclosing IPs
print(length(unique(sw_version_23[,2]))/length(unique(scans_23[,2])))

# Day by day scan
y_23 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Disclosing", "Total", "Fraction"))
dates <- seq(as.Date("2018-01-01"), as.Date("2018-09-01"), by=1)

ii <-0

for (d in  seq_along(dates)){
  
  # Total entries for the day
  sw_tot_23 <- dplyr::filter(scans_23, grepl(dates[[d]], timestamp))
  sw_tot_u_23 <- unique(sw_tot_23[,2])
  
  # Total entries for the day matching the regex
  sw_disc_23 <- dplyr::filter(sw_version_23, grepl(dates[[d]], timestamp))
  sw_disc_u_23 <- unique(sw_disc_23[,2])
  
  # Fraction of disclosing IPs
  sw_fraction_23 <- length(sw_disc_u_23)/length(sw_tot_u_23)
  
  new_row <- data.frame(Discolosing=length(sw_disc_u_23),Total=length(sw_tot_u_23),Fraction=sw_fraction_23)
  y_23 <- rbind(y_23,new_row)
  
  print(sw_fraction_23)
  ii <- ii+1
  print(ii)
}

# Remove empty days
y_23 <- na.omit(y_23)
# Mean of daily percentages
mean <- mean(y_23[["Fraction"]])

View(y_23)
View(mean)


# See for each IP that discloses, how many times it actually discloses compared to how many times it is scanned
x_23 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("IP", "Port", "Fraction"))
ips_23 <- unique(sw_version_23[,2])
ii <- 0
for (i in ips_23){
  print(i)
  # Total entries for the day
  sw_tot_23 <- subset(scans_23, ip == i)
  
  # Total entries for the day matching the regex
  sw_disc_23 <- subset(sw_version_23, ip == i)
  
  # Percentage of disclosing scans
  sw_fraction <- nrow(sw_disc_23)/nrow(sw_tot_23)
  
  new_row <- data.frame(IP=i,Port=23,Fraction=sw_fraction)
  x_23 <- rbind(x_23,new_row)
  ii <- ii+1
  print(ii)
}

View(x_23)




# PORT2323 #######################################################################################
# Scans on port 2323
scans_2323 <- subset(scans, port == 2323)
# Messages disclosing information on port 2323
sw_version_2323 <- subset(sw_version, port == 2323)

# Total number of unique IPs scanned on port 2323
print(length(unique(scans_2323[,2])))
# Total number of unique IPs disclosing information on port 2323
print(length(unique(sw_version_2323[,2])))
# Fraction of disclosing IPs
print(length(unique(sw_version_2323[,2]))/length(unique(scans_2323[,2])))

# Day by day scan
y_2323 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Disclosing", "Total", "Fraction"))
dates <- seq(as.Date("2018-01-01"), as.Date("2018-09-01"), by=1)

ii <-0

for (d in  seq_along(dates)){
  
  # Total entries for the day
  sw_tot_2323 <- dplyr::filter(scans_2323, grepl(dates[[d]], timestamp))
  sw_tot_u_2323 <- unique(sw_tot_2323[,2])
  
  # Total entries for the day matching the regex
  sw_disc_2323 <- dplyr::filter(sw_version_2323, grepl(dates[[d]], timestamp))
  sw_disc_u_2323 <- unique(sw_disc_2323[,2])
  
  # Fraction of disclosing IPs
  sw_fraction_2323 <- length(sw_disc_u_2323)/length(sw_tot_u_2323)
  
  new_row <- data.frame(Discolosing=length(sw_disc_u_2323),Total=length(sw_tot_u_2323),Fraction=sw_fraction_2323)
  y_2323 <- rbind(y_2323,new_row)
  
  print(sw_fraction_2323)
  ii <- ii+1
  print(ii)
}

# Remove empty days
y_2323 <- na.omit(y_2323)
# Mean of daily percentages
mean <- mean(y_2323[["Fraction"]])

y_23_cp <- y_23
y_2323_cp <- y_2323
test <- rbind(y_23_cp,y_2323_cp)
mean_tot <- mean(test[["Fraction"]])
View(mean_tot)

# See for each IP that discloses, how many times it actually discloses compared to how many times it is scanned
x_2323 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("IP", "Port", "Fraction"))
ips_2323 <- unique(sw_version_2323[,2])
ii <- 0
for (i in ips_2323){
  print(i)
  # Total entries for the day
  sw_tot_2323 <- subset(scans_2323, ip == i)
  
  # Total entries for the day matching the regex
  sw_disc_2323 <- subset(sw_version_2323, ip == i)
  
  # Percentage of disclosing scans
  sw_fraction <- nrow(sw_disc_2323)/nrow(sw_tot_2323)
  
  new_row <- data.frame(IP=i,Port=2323,Fraction=sw_fraction)
  x_2323 <- rbind(x_2323,new_row)
  ii <- ii+1
  print(ii)
}

View(x_2323)


# PORT23 and PORT2323 #######################################################################################

# Total number of unique IPs scanned on port 2323
print(length(unique(scans_23[,2]))+length(unique(scans_2323[,2])))
# Total number of unique IPs disclosing information on port 2323
print(length(unique(sw_version_23[,2]))+length(unique(sw_version_2323[,2])))
# Fraction of disclosing IPs
print((length(unique(sw_version_23[,2]))+length(unique(sw_version_2323[,2])))/(length(unique(scans_23[,2]))+length(unique(scans_2323[,2]))))

















