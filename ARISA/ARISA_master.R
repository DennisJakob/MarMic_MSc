# Master script for ARISA analysis

# imports merged ARISA data and meta data
source("importData.R")
# standardizes merged ARISA data with hellinger methode
source("standardization.R")
# creates dissimilarity matrices
source("dissimilarity.R")
# performes hierarchcal cluster analysis
source("hierarchicalCluster.R")
# performes anosim analysis
source("anosim.R")
# performs NMDS on ARISA dissimilarity
source("nmds.R")
# plots NMDS plots
source("nmdsPlots.R")
# plots hierarchical cluster analysis plots (dendrograms)
source("hcPlots.R")
