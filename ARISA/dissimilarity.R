# Script to generate the Bray-Curtis and Jaccard dissimilarity matrices
require("vegan")
require("reshape2")

# Generate Bray-Curtis matrix
data.hel.bc <- vegdist(
  data.hel,
  method = "bray",
)

data.exp.hel.bc <- vegdist(
  data.exp.hel,
  method = "bray",
)

# Generate Bray-Curtis matrix
data.hel.jac <- vegdist(
  data.hel,
  method = "jaccard",
  binary = T # always use binary = TRUE for traditional jaccard interpretation
)

data.exp.hel.jac <- vegdist(
  data.exp.hel,
  method = "jaccard",
  binary = T
)

# Jaccard by hand
# melt dataframe
data.exp.hel.jac2 <- melt(as.matrix(data.exp.hel.jac), varnames = c("row", "col"))
write.csv(data.exp.hel.jac2, "turnover.csv")

# check for presence-absence 
data.exp.pa <- decostand(data.exp, method = "pa") 

# calculate Jaccard index
temp <- data.exp.pa[c("5_fed_1","0_fed_1"), ] %>% colSums()
sum(temp == 1) / (sum(temp == 1) + sum(temp == 2))

# plot alpha diversity
# Richness
data.exp.inext.q0 <- iNEXT(t(data.exp), q = 0, knots = 50, nboot = 0)
png("richness.png")
plot(data.exp.inext.q0)
dev.off()

 # Inverse Simpson
 data.exp.inext.q2 <- iNEXT(t(data.exp), q = 2, knots = 50, nboot = 0)
 png("inverseSimpson.png")
 plot(data.exp.inext.q2)
 dev.off()

