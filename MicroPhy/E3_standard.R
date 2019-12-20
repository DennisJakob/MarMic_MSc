# script for processing standards of colorimetric data 
# marmic practical - microbial physiology - experiment 2

# data import
standard <- read.delim("E3_standard.txt", sep = "\t", dec = ",")

# calculating mean and standard deviation

# NH4
NH4 <- standard[33:48,c(1,2,5,6,7,8)]
NH4_mean <- as.data.frame(NH4[,2])
for (i in 1:nrow(NH4_mean)) {
  NH4_mean[i,2] <- mean(c(NH4[i,3], NH4[i,4], NH4[i,5], NH4[i,6]))
  #NH4_mean[i,3] <- sd(c(NH4[i,3], NH4[i,4], NH4[i,5], NH4[i,6]))
}
colnames(NH4_mean) <- c("concentration",
                        "OD"
                        #,"SD"
                        )
NH4_mean <- aggregate(data=NH4_mean, OD~., FUN=mean)
# blanc substraction
NH4_blanc <- NH4_mean$OD[1]
NH4_mean$OD <- NH4_mean$OD-NH4_mean$OD[1]
# linear model
NH4_lm <- lm(OD ~ 0 + concentration, data=NH4_mean)
summary(NH4_lm)
NH4_coef <- NH4_lm$coefficients[[1]]
plot(x=NH4_mean$concentration,
     y=NH4_mean$OD,
     main = "standard curve NH4+",
     xlab = "c(NH4+) [uM]",
     ylab = "OD650")
abline(NH4_lm)


# N02 measurement 1
NO2_1 <- standard[1:16,c(1,2,5,6,7,8)]
NO2_1_mean <- as.data.frame(NO2_1[,2])
for (i in 1:nrow(NO2_1_mean)) {
  NO2_1_mean[i,2] <- mean(c(NO2_1[i,3], NO2_1[i,4], NO2_1[i,5], NO2_1[i,6]))
}
colnames(NO2_1_mean) <- c("concentration", "OD")
NO2_1_mean <- aggregate(data=NO2_1_mean, OD~., FUN=mean)
# blanc substraction
NO2_1_blanc <- NO2_1_mean$OD[1]
NO2_1_mean$OD <- NO2_1_mean$OD-NO2_1_mean$OD[1]
# linear model
NO2_1_lm <- lm(OD ~ 0 + concentration, data=NO2_1_mean)
summary(NO2_1_lm)
NO2_1_coef <- NO2_1_lm$coefficients[[1]]
plot(x=NO2_1_mean$concentration,
     y=NO2_1_mean$OD,
     main = "standard curve NO2-",
     xlab = "c(NO2-) [uM]",
     ylab = "OD540")
abline(NO2_1_lm)


# N03 measurement 1
NO3_1 <- standard[17:32,c(1,2,5,6,7,8)]
NO3_1_mean <- as.data.frame(NO3_1[,2])
for (i in 1:nrow(NO3_1_mean)) {
  NO3_1_mean[i,2] <- mean(c(NO3_1[i,3], NO3_1[i,4], NO3_1[i,5], NO3_1[i,6]))
}
colnames(NO3_1_mean) <- c("concentration", "OD")
NO3_1_mean <- aggregate(data=NO3_1_mean, OD~., FUN=mean)
# blanc substraction
NO3_1_blanc <- NO3_1_mean$OD[1]
NO3_1_mean$OD <- NO3_1_mean$OD-NO3_1_mean$OD[1]
# linear model
NO3_1_lm <- lm(OD ~ 0 + concentration, data=NO3_1_mean)
summary(NO3_1_lm)
NO3_1_coef <- NO3_1_lm$coefficients[[1]]
plot(x=NO3_1_mean$concentration,
     y=NO3_1_mean$OD,
     main = "standard curve NO3-",
     xlab = "c(NO2-) [uM]",
     ylab = "OD540")
abline(NO3_1_lm)


# N02 measurement 2
NO2_2 <- standard[1:16,c(1,2,9,10,11,12)]
NO2_2_mean <- as.data.frame(NO2_2[,2])
for (i in 1:nrow(NO2_2_mean)) {
  NO2_2_mean[i,2] <- mean(c(NO2_2[i,3], NO2_2[i,4], NO2_2[i,5], NO2_2[i,6]))
}
colnames(NO2_2_mean) <- c("concentration", "OD")
NO2_2_mean <- aggregate(data=NO2_2_mean, OD~., FUN=mean)
# blanc substraction
NO2_2_blanc <- NO2_2_mean$OD[1]
NO2_2_mean$OD <- NO2_2_mean$OD-NO2_2_mean$OD[1]
# linear model
NO2_2_lm <- lm(OD ~ 0 + concentration, data=NO2_2_mean)
summary(NO2_2_lm)
NO2_2_coef <- NO2_2_lm$coefficients[[1]]
plot(x=NO2_2_mean$concentration,
     y=NO2_2_mean$OD,
     main = "standard curve NO2- red",
     xlab = "c(NO2-) [uM]",
     ylab = "OD540")
abline(NO2_2_lm)


# N03 measurement 2
NO3_2 <- standard[17:32,c(1,2,9,10,11,12)]
NO3_2_mean <- as.data.frame(NO3_2[,2])
for (i in 1:nrow(NO3_2_mean)) {
  NO3_2_mean[i,2] <- mean(c(NO3_2[i,3], NO3_2[i,4], NO3_2[i,5], NO3_2[i,6]))
}
colnames(NO3_2_mean) <- c("concentration", "OD")
NO3_2_mean <- aggregate(data=NO3_2_mean, OD~., FUN=mean)
# blanc substraction
NO3_2_blanc <- NO3_2_mean$OD[1]
NO3_2_mean$OD <- NO3_2_mean$OD-NO3_2_mean$OD[1]
# linear model
NO3_2_lm <- lm(OD ~ 0 + concentration, data=NO3_2_mean)
summary(NO3_2_lm)
NO3_2_coef <- NO3_2_lm$coefficients[[1]]
plot(x=NO3_2_mean$concentration,
     y=NO3_2_mean$OD,
     main = "standard curve NO3- red",
     xlab = "c(NO2-) [uM]",
     ylab = "OD540")
abline(NO3_2_lm)

