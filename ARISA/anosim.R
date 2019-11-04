# Hypothesis testing

require("vegan")

# preliminary check, that the names in metadata and data objects match and are in the same order
all.equal(
  row.names(data.exp.hel),
  row.names(metadata.exp)
)
# if TRUE --> nice

#ANOSIM
set.seed(123)
anosim(
  x = data.exp.hel.bc,
  grouping = metadata.exp$Temperature
)

# Call:
#   anosim(x = data.exp.bc, grouping = metadata.exp$Temperature) 
# Dissimilarity: bray 
# 
# ANOSIM statistic R: 0.375 
# Significance: 0.003 
# 
# Permutation: free
# Number of permutations: 999

set.seed(123)
data.exp.hel.bc.anosim.Temp <- anosim(
  x = data.exp.hel.bc,
  grouping = as.character(metadata.exp$Temperature)
)
png("anosim_temperature.png")
plot(data.exp.hel.bc.anosim.Temp,
     main = "Temperature")
dev.off()

# Call:
#   anosim(x = data.exp.bc, grouping = metadata.exp$Treatment) 
# Dissimilarity: bray 
# 
# ANOSIM statistic R: 0.4046 
# Significance: 0.001 
# 
# Permutation: free
# Number of permutations: 999

set.seed(123)
data.exp.hel.bc.anosim.Treat <- anosim(
  x = data.exp.hel.bc,
  grouping = as.character(metadata.exp$Treatment)
)
png("anosim_treatment.png")
plot(data.exp.hel.bc.anosim.Treat,
     main = "Treatment")
dev.off()

# ADONIS
set.seed(123)
data.exp.hel.adonis <- adonis(
  data.exp.hel ~ Treatment*Temperature,
  data = metadata.exp,
  method = "bray")

# Call:
#   adonis(formula = data.exp ~ Treatment * Temperature, data = metadata.exp,      method = "bray") 
# 
# Permutation: free
# Number of permutations: 999
# 
# Terms added sequentially (first to last)
# 
# Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# Treatment              1    1.0751 1.07508  8.4183 0.27326  0.001 ***
#   Temperature            1    0.8755 0.87546  6.8552 0.22252  0.001 ***
#   Treatment:Temperature  1    0.4512 0.45123  3.5333 0.11469  0.007 ** 
#   Residuals             12    1.5325 0.12771         0.38952           
# Total                 15    3.9343                 1.00000           
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

