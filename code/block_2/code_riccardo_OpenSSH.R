#---> OpenSSH version status <---

# Load csv file
scans <- read.csv("~/Downloads/telnetscan.csv")
# Select all the rows with OpenSSH in the banner column
os <- dplyr::filter(scans, grepl('OpenSSH', banner))



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
