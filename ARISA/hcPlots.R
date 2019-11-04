# Script for hierarchical cluster analysis plots
require("ggplot2")
require("ggdendro")

plot(
  data.hel.bc.hc
)

plot(
  data.exp.hel.bc.hc,
  main = "Average Clustering of Bray-Curtis dissimilarities"
)

# ggplot2
d.data <- dendro_data(as.dendrogram(data.exp.hel.bc.hc), type = "rectangle")
png("dendrogram.png")
plot.d <- ggplot(segment(d.data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = label(d.data), 
            aes(x = x, y = y, label = label), hjust = -0.1, size = 3) +
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()