# Script for hierarchical cluster analysis

# Brauy-Curtis
data.hel.bc.hc <- hclust(
  d = data.hel.bc,
  method = "average"
)

data.exp.hel.bc.hc <- hclust(
  d = data.exp.hel.bc,
  method = "average"
)

# Jaccard
data.hel.jac.hc <- hclust(
  d = data.hel.jac,
  method = "average"
)

data.exp.hel.jac.hc <- hclust(
  d = data.exp.hel.jac,
  method = "average"
)
