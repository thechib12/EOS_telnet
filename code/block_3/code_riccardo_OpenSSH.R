#---> OpenSSH version status <---
library(ggplot2)
# Load csv file
scans <- read.csv("~/Documents/Uni/UT/_EoS/_assignments/scan/telnetscan.csv")
# Select all the rows with OpenSSH in the banner column
os <- dplyr::filter(scans, grepl("[O|o]pen[S|s][S|s][H|h]", banner))



## Number of unique IPs running OpenSSH
os_unique <- unique(os[,2])
length(os_unique)



## IPs running a specific OpenSSH version
os_vers <- data.frame()
## IPs already considered
os_done <- data.frame()



# IPs running 6.9 < x < 7.8
for (i in 7:0){
  os_num <- dplyr::filter(os, grepl(paste("7.",i, sep = ""), banner))
  os_num_unique <- unique(os_num[,2])
  os_num_unique <- setdiff(os_num_unique,os_done)
  os_vers <- rbind(os_vers,length(os_num_unique))
  os_done <- union(os_done,os_num_unique)
}



# IPs running x < 7.X
os_prev7 <- dplyr::filter(os, grepl('1.|2.|3.|4.|5.|6.', banner))
os_prev7_unique <- unique(os_prev7[,2])
os_prev7_unique <- setdiff(os_prev7_unique,os_done)
os_vers <- rbind(os_vers,length(os_prev7_unique))




#---> SSH day by day <---

sw_version <- dplyr::filter(scans, grepl("[O|o]pen[S|s][S|s][H|h]", banner))

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
y_23 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SSH", "Total", "Fraction"))
dates <- seq(as.Date("2018-01-01"), as.Date("2018-09-01"), by=1)

ii <-0

for (d in  seq_along(dates)){
  
  # Total entries for the day
  sw_tot_23 <- dplyr::filter(scans_23, grepl(dates[[d]], timestamp))
  sw_tot_u_23 <- unique(sw_tot_23[,2])
  
  # Total entries for the day matching the regex
  sw_disc_23 <- dplyr::filter(sw_version_23, grepl(dates[[d]], timestamp))
  sw_disc_u_23 <- unique(sw_disc_23[,2])
  
  # Fraction of SSH IPs
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
y_2323 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SSH", "Total", "Fraction"))
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


# PORT23 and PORT2323 #######################################################################################

y_plot <- y_23

for (i in 1:244){
  y_plot$Discolosing[i] = y_23$Discolosing[i] + y_2323$Discolosing[i]
  y_plot$Total[i] = y_23$Total[i] + y_2323$Total[i]
  y_plot$Fraction[i] = y_plot$Discolosing[i] / y_plot$Total[i]
}

y_plot <- na.omit(y_plot)
print(nrow(y_plot))
g <- data.frame("Day" = 1:229)
y_plot <- cbind(y_plot,g)
View(y_plot)

ggplot(data=y_plot, aes(x=Day, y=Fraction, group=1)) + geom_line()

















