# script for analysis of colorimetric data 
# marmic practical - microbial physiology - experiment 2

# for processing of standards in seperate script
source("E3_standard.R")

# data import
ammonium <- read.delim("E3_ammonium.txt", sep = "\t", dec = ",") # ammonium measurement
nitrite_1 <- read.delim("E3_nitrite_1.txt", sep = "\t", dec = ",") # nitrite measurement
nitrite_2 <- read.delim("E3_nitrite_2.txt", sep = "\t", dec = ",") # nitrite measurement after reduction
meta <- read.delim("E3_meta.txt", sep = "\t", dec = ",") # experimental meta data
meta_sub <- subset(meta, !is.na(meta$dilution.factor)) # subset of meta data

# ammonium concentrations
ammonium_mean <- as.data.frame(ammonium$sample) # create dataframe to average values
for (i in 1:nrow(ammonium_mean)) {
  ammonium_mean[i,2] <- mean(c(ammonium[i,4], ammonium[i,5], ammonium[i,6], ammonium[i,7])) # calculate mean
  ammonium_mean[i,3] <- sd(c(ammonium[i,4], ammonium[i,5], ammonium[i,6], ammonium[i,7])) # calculate standard deviation
}
colnames(ammonium_mean) <- c("sample", "OD", "SD")
ammonium_mean$OD <- ammonium_mean$OD-NH4_blanc # substract blanc
ammonium_conc <- as.data.frame(ammonium_mean$sample) # create dataframe for ammonium concentrations in samples
ammonium_conc[,2] <- ammonium_mean$OD/NH4_coef # calculate concentration
colnames(ammonium_conc) <- c("sample", "concentration")

# nitrite
nitrite_mean <- as.data.frame(nitrite_1$sample)
for (i in 1:nrow(nitrite_mean)) {
  nitrite_mean[i,2] <- mean(c(nitrite_1[i,4], nitrite_1[i,5], nitrite_1[i,6], nitrite_1[i,7]), na.rm=T)
  nitrite_mean[i,3] <- sd(c(nitrite_1[i,4], nitrite_1[i,5], nitrite_1[i,6], nitrite_1[i,7]))
}
colnames(nitrite_mean) <- c("sample", "OD", "SD")
nitrite_mean$OD <- nitrite_mean$OD-NO2_1_blanc
nitrite_conc <- as.data.frame(nitrite_mean$sample)
nitrite_conc[,2] <- nitrite_mean$OD/NO2_1_coef
colnames(nitrite_conc) <- c("sample", "concentration")

# nitrate
nitrate_mean <- as.data.frame(nitrite_2$sample)
for (i in 1:nrow(nitrate_mean)) {
  nitrate_mean[i,2] <- mean(c(nitrite_2[i,4], nitrite_2[i,5], nitrite_2[i,6], nitrite_2[i,7]), na.rm=T)
  nitrate_mean[i,3] <- sd(c(nitrite_2[i,4], nitrite_2[i,5], nitrite_2[i,6], nitrite_2[i,7]))
}
colnames(nitrate_mean) <- c("sample", "OD", "SD")
nitrate_mean$OD <- nitrate_mean$OD-NO3_2_blanc
nitrate_conc <- as.data.frame(nitrate_mean$sample)
nitrate_conc[,2] <- (nitrate_mean$OD-NO2_2_coef*nitrite_conc$concentration)/NO3_2_coef
colnames(nitrate_conc) <- c("sample", "concentration")

# combine results
conc <- cbind(ammonium_conc, nitrite_conc$concentration, nitrate_conc$concentration) # combine calculated concentrations
colnames(conc) <- c("sample", "c(NH4+)", "c(NO2-)", "c(NO3-)")
data <- merge(x = meta_sub, y = conc, by = "sample") # merge with meta data
data[,13:15] <- data[,10:12]*data$dilution.factor
colnames(data)[13:15] <- c("c(NH4+) sample", "c(NO2-) sample", "c(NO3-) sample")

write.table(data, "E3_results.txt", sep = "\t", row.names=F) # export table as tab separated file
