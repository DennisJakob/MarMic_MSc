require("tidyverse")
# script for processing standards of gas chromatography data
# marmic practical - microbial physiology - experiment 1

# for processing of standards in seperate script
source("E1_standard.R")

# data import
methane <- read.delim("E1_methane.txt", sep = "\t", dec = ",") # methane measurement
meta <- read.delim("E1_meta.txt", sep = "\t", dec = ",") # experimental meta data

OD <- 1
sample <- c("DL1",
            "DL2",
            "DL3",
            "DL4")

methane_p <- cbind(methane$time..min., methane[,2:5]/methane_coef)
colnames(methane_p) <- c("time..min.",
                         "DL1", # [%]
                         "DL2", # [%]
                         "DL3", # [%]
                         "DL4") # [%]

# linear models
DL1_lm <- lm(DL1 ~ time..min., data=methane_p)
DL1_int <- DL1_lm$coefficients[[1]]
DL1_coef <- DL1_lm$coefficients[[2]]
plot(x=methane_p$time..min.,
     y=methane_p$DL1,
     main = "methane regression DL1",
     xlab = "time [min]",
     ylab = "c(methane) [%]")
abline(DL1_lm)

DL2_lm <- lm(DL2 ~ time..min., data=methane_p)
DL2_int <- DL2_lm$coefficients[[1]]
DL2_coef <- DL2_lm$coefficients[[2]]
plot(x=methane_p$time..min.,
     y=methane_p$DL2,
     main = "methane regression DL2",
     xlab = "time [min]",
     ylab = "c(methane) [%]")
abline(DL2_lm)

DL3_lm <- lm(DL3 ~ time..min., data=methane_p)
DL3_int <- DL3_lm$coefficients[[1]]
DL3_coef <- DL3_lm$coefficients[[2]]
plot(x=methane_p$time..min.,
     y=methane_p$DL3,
     main = "methane regression DL3",
     xlab = "time [min]",
     ylab = "c(methane) [%]")
abline(DL3_lm)

DL4_lm <- lm(DL4 ~ time..min., data=methane_p)
DL4_int <- DL4_lm$coefficients[[1]]
DL4_coef <- DL4_lm$coefficients[[2]]
plot(x=methane_p$time..min.,
     y=methane_p$DL4,
     main = "methane regression DL4",
     xlab = "time [min]",
     ylab = "c(methane) [%]")
abline(DL4_lm)

# results

intercept <- c(DL1_lm$coefficients[[1]], DL2_lm$coefficients[[1]], DL3_lm$coefficients[[1]], DL4_lm$coefficients[[1]])
slope <- c(DL1_lm$coefficients[[2]], DL2_lm$coefficients[[2]], DL3_lm$coefficients[[2]], DL4_lm$coefficients[[2]])
phys_rate <- slope*60/OD
lm <- rbind(sample, intercept, slope, phys_rate)
lm <- as.data.frame(t(lm))
data <- merge(x = meta, y = lm, by = "sample") # merge with meta data

write.table(data, "E1_results.txt", sep = "\t", row.names=F) # export table as tab separated file
