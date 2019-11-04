# Script for standardization
require("vegan")

# Hellinger standardization

data.hel <- decostand(
  x = data,
  method = "hel",
  MARGIN = 1
)

data.exp.hel <- decostand(
  x = data.exp,
  method = "hel",
  MARGIN = 1
)
