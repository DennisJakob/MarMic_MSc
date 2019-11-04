# Import for ARISA data and contextualizing metadata

require("tidyverse")

# Import merged ARISA_merged table
data <- read.table(
  'ARISA_merged.txt',
  header = T,
  row.names = 1,
  sep = "\t"
)

# Import metadata
metadata <- read.table(
  'EnvData_merged.txt',
  header = T,
  row.names = 1,
  sep = "\t"
)

# Derive experimental subset
data %>% row.names %>% grep("fed", ., value = T) -> experimentalRows
data.exp <- data[experimentalRows,]
metadata.exp <- metadata[experimentalRows,]

