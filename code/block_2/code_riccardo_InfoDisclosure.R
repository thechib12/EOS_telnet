#---> Find IPs disclosing information on running software <---

# Loading of csv file
scan <- read.csv("~/Downloads/telnetscan.csv")

# Number of unique IPs over the whole dataset
ip_unique <- unique(scan[,2])

# Isolating messages giving information about running software/services
# (v?[0-9][0-9]?\\.[0-9][0-9]?) is meant to caputre software versions (e.g., v2.1, 3.23..)
sw_version <- dplyr::filter(scan, grepl("(v?[0-9][0-9]?\\.[0-9][0-9]?)|RICOH|Huawei|FTP|SMTP|Linux|Microsoft|Cisco|Broadband|NNTP|NNRP|Debian|Raspbian|openpli|DiskStation|Router|ZyXEL|SRX[0-9]+|VMG[0-9]+|NBG[0-9]+|FVS[0-9]+|RT[0-9]+|ZEM[0-9]+|UR5", banner))

# Number of unique IPs disclosing information
sw_version <- unique(sw_version[,2])
