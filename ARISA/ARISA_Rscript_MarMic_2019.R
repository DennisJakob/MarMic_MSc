#input:
# geneMapper output table
# mapping file to match PCR number and sample name

## Notes CB 28/10/2019 
## Following samples were removed from further analyses, due to bad quality of the size standard and/or fragment run:  
## PCR_numbers: 988, 1020, 1028, 1044, 1052; 
## correspond to samples: 0_unfed_2b, 28_unfed_1a, 28_fed_1c, Chitin_10,6mg_1a, Chitin_106mg_1c 
## these samples were not output from GeneMapper 

setwd("//groups/biogeo/habitat/MarMic Praktikum/2019/Results/ARISA/R_analyses")

save.image("MarMic19_ARISA.Rdata")
load("MarMic17_ARISA.Rdata")

#reading geneMapper output (export table), in Size column often NAs
Data  <- read.table(
  file = "Marmic2019_GM_output.txt",
  h = TRUE, # is there a header?
  fill = TRUE,
  sep = "\t"
  )


#selecting columns of interest
Data1 <- Data[ , c("Sample.File.Name","Size","Area")] 

#removing NAs from Size column
Data2 <- Data1[!is.na(Data1$Size),] 

#defining size range (redundant)
Data3 <- Data2[Data2$Size >= 100 & Data2$Size <= 1000, ] 

#shortening the names of the samples ####depending on the naming of frags!!!!!!
ShortNames <- as.character(
  sapply(
    as.character(Data3$Sample.File.Name),
    function(x){
      strsplit(x,"_2017-")[[1]][1]
      }
    )
  )

Data4 <- Data3
Data4$Sample.File.Name <- ShortNames

head(Data4)
tail(Data4)

write.table(Data4,"for_binning.txt")

# binning, using a script that was written by
# A. Ramette to bin ARISA data.
source("interactive_binner v1.4_abs.r")

Binned <- interactivebinner(
  Data4,
  absolute = T # absolute option also outputs 
               # the raw peak area values per bin
  ) 
###
#Best bin frame is:  1.1  (highest mean correlations)
#Max OTU number for frame:  1.7 	(431 OTUs)
#(Chosen parameters: WS=2, Shift=0.1)
#The results are available in the following object: Result

#Result summary for each frame:
#  0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9      1    1.1    1.2    1.3    1.4    1.5    1.6    1.7
#Correlations   0.21   0.22   0.22   0.22   0.23   0.23   0.22   0.21   0.21   0.23   0.27   0.28   0.28   0.28   0.28   0.27   0.23   0.21
#NberOTUs     429.00 430.00 428.00 430.00 429.00 426.00 427.00 427.00 426.00 424.00 424.00 426.00 426.00 426.00 429.00 428.00 430.00 431.00
#1.8   1.9
#Correlations   0.2   0.2
#NberOTUs     427.0 430.0

###
B <- Binned$BestFrame
write.table(B,"binned.txt")

#reading mapping file to match PCR number, i.e. rownames(B), with sample names (SID)
SID0 <- read.table("SID_trip.txt", h = TRUE) 
head(SID0)

PCR <- as.numeric(
  sapply(as.character(rownames(B)),
         function(x){strsplit(x,"_")[[1]][2]}
         )
  )

SID <- SID0[SID0$PCR %in% PCR, ]

SID <- SID[match(PCR,SID$PCR_number  ),]

#formating input for quality check
M_trip <- data.frame(B,G = SID$Sample_ID_triplicates)
rownames(M_trip)=SID$Sample_ID_triplicates
write.table(M_trip, "Binned_wcontrols.txt", sep="\t", quote = F) 

###data ready for further analysis, but it can also be checked for quality

#reading mapping file to match PCR number, i.e. rownames(B), with sample names (SID)
SID0 <- read.table("SID_merged.txt", h = TRUE) 
head(SID0)

PCR <- as.numeric(
  sapply(as.character(rownames(B)),
         function(x){strsplit(x,"_")[[1]][2]}
  )
)

SID <- SID0[SID0$PCR %in% PCR, ]

SID <- SID[match(PCR,SID$PCR_number),]

#formating input for merging
M <- data.frame(B,G = SID$Sample_ID_merged)


#merging replicate PCRs into 1 sample
#OTU must be present in at least 2 replicate PCRs
source("replicate_merger_ALk_consensus_RFI 1.2.r") #mean of proportions
Merged <- Merging_ALk(M, k = 2)

write.table(Merged, "ARISA_merged.txt", sep="\t", quote = F) 

## ARISA_merged.txt still contains all controls (positive and negative PCR controls, and different amounts of chitin)
## row 18-23 chitin, row 24-29 PCR controls, row 30-31 t0 