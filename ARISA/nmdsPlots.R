# Script for plotting NMDS

require("vegan")
require("ggplot2")
require("grid")
require("gridExtra")

# extract NMDS scores and groups from data tables
data.exp.hel.bc.nmds.scores <- as.data.frame(scores(data.exp.hel.bc.nmds))
data.exp.hel.bc.nmds.scores$Treatment <- as.factor(metadata.exp$Treatment)
data.exp.hel.bc.nmds.scores$Temperature <- as.factor(metadata.exp$Temperature)

data.hel.bc.nmds.scores <- as.data.frame(scores(data.hel.bc.nmds))
data.hel.bc.nmds.scores$Treatment <- as.factor(metadata$Treatment)
data.hel.bc.nmds.scores$Temperature <- as.factor(metadata$Temperature)

# hull values for temperatures
T0 <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "0", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "0", c("NMDS1", "NMDS2")]), ]  
T5 <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "5", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "5", c("NMDS1", "NMDS2")]), ]  
T18 <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "18", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "18", c("NMDS1", "NMDS2")]), ]  
T28 <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "28", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Temperature == "28", c("NMDS1", "NMDS2")]), ]  
hull.data.temperature <- rbind(T0, T5, T18, T28)

# hull values for treatments
fed <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Treatment == "fed", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Treatment == "fed", c("NMDS1", "NMDS2")]), ]  
unfed <- data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Treatment == "unfed", ][chull(data.exp.hel.bc.nmds.scores[data.exp.hel.bc.nmds.scores$Treatment == "unfed", c("NMDS1", "NMDS2")]), ]  
hull.data.treatment <- rbind(fed, unfed)

# NMDS plots
col_list1 <- c("0" = "#2c7bb6", "5" = "#abd9e9", "18" = "#fdae61", "28" = "#d7191c")
col_list2 <- c("fed" = "#bdbdbd", "unfed" = "#636363")

# Controls included
png("nmds_controls.png")
ggplot() + 
  geom_point(data=data.hel.bc.nmds.scores,
             aes(x=NMDS1,y=NMDS2,shape=Treatment,colour=Temperature),
             size=3) +
  scale_colour_manual(values = col_list1,
                      na.value = "#636363") +
  annotation_custom(grob = textGrob(paste("stress = ", round(data.hel.bc.nmds[["stress"]], digits = 3), sep = "")),  
                    xmin = 0.3, xmax = 0.425, ymin = -0.33, ymax = -0.33) +
  theme_bw()
dev.off()

# Temperature
png("nmds_temperature.png")
ggplot() + 
  geom_polygon(data=hull.data.temperature,
               aes(x=NMDS1,y=NMDS2,fill=Temperature,group=Temperature),
               alpha=0.3) +
  geom_point(data=data.exp.hel.bc.nmds.scores,
             aes(x=NMDS1,y=NMDS2,shape=Treatment,colour=Temperature),
             size=3) +
  scale_colour_manual(values = col_list1) +
  scale_fill_manual(values = col_list1) +
  annotation_custom(grob = textGrob(paste("stress = ", round(data.exp.hel.bc.nmds[["stress"]], digits = 3), sep = "")),  
                    xmin = 0.3, xmax = 0.425, ymin = -0.33, ymax = -0.33) +
  theme_bw()
dev.off()

# Treatment
png("nmds_treatment.png")
ggplot() + 
  geom_polygon(data=hull.data.treatment,
               aes(x=NMDS1,y=NMDS2,fill=Treatment,group=Treatment),
               alpha=0.3) +
  geom_point(data=data.exp.hel.bc.nmds.scores,
             aes(x=NMDS1,y=NMDS2,shape=Treatment,colour=Temperature),
             size=3) +
  scale_colour_manual(values = col_list1) +
  scale_fill_manual(values = col_list2) +
  annotation_custom(grob = textGrob(paste("stress = ", round(data.exp.hel.bc.nmds[["stress"]], digits = 3), sep = "")),  
                    xmin = 0.3, xmax = 0.425, ymin = -0.33, ymax = -0.33) +
  theme_bw()
dev.off()
