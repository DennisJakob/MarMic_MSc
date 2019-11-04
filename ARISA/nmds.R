# Perform an NMDS on ARISA dissimilarity

require("vegan")

# Create NMDS object
# Bray-Curtis
data.hel.bc.nmds <- metaMDS(data.hel.bc)
data.exp.hel.bc.nmds <- metaMDS(data.exp.hel.bc)

# Jaccard
data.hel.jac.nmds <- metaMDS(data.hel.jac)
data.exp.hel.jac.nmds <- metaMDS(data.exp.hel.jac)

