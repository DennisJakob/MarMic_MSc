# script for processing standards of colorimetric data 
# marmic practical - microbial physiology - experiment 2

# data import
axenic <- read.delim("TW_axenic.txt", sep = "\t", dec = ",")
axenic <- axenic[3:13,2:7]
nonaxenic <- read.delim("TW_nonaxenic.txt", sep = "\t", dec = ",")
nonaxenic <- nonaxenic[3:13,2:7]
