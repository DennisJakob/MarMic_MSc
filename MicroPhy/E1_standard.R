# script for processing standards of gas chromatography data
# marmic practical - microbial physiology - experiment 1

# data import
standard <- read.delim("E1_standard.txt", sep = "\t", dec = ",")

# calculating mean and standard deviation

# methane

for (i in 1:nrow(standard)) {
  standard[i,5] <- mean(c(standard[i,2], standard[i,3], standard[i,4]))
  standard[i,6] <- sd(c(standard[i,2], standard[i,3], standard[i,4]))
}
colnames(standard) <- c("concentration", # [%]
                        "rep1",
                        "rep2",
                        "rep3",
                        "mean",
                        "sd")

# linear model
methane_lm <- lm(mean ~ 0 + concentration, data=standard)
summary(methane_lm)
methane_coef <- methane_lm$coefficients[[1]]
plot(x=standard$concentration,
     y=standard$mean,
     main = "standard curve methane",
     xlab = "c(methane) [%]",
     ylab = "area")
abline(methane_lm)