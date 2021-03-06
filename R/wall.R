library(ggplot2)
library(pheatmap)
library(RColorBrewer)

disease <- "Diabetes_mellitus"

#png(filename = paste0(paste0("GitHub/swotr/www/",disease),"_wall.png"),
#    type = "cairo",
#    units="in",
#    width = 13.75,
#    height = 1.38,
#    pointsize = 10,
#    res = 580)
png(filename = paste0(disease,"_wall.png"),
    type = "cairo",
    units="in",
    width = 13.75,
    height = 1.38,
    pointsize = 10,
    res = 580)

# get connection data to determine the width of heatmap parts
#dat <- read.csv(paste0(paste0("GitHub/swotr/data/",disease),"_connection_data.csv"), sep = ",", head = FALSE);
# Import heat connection data from SWOT clock site
#heat <- read.csv(paste0(paste0("GitHub/swotr/data/", disease), "_heat_map_data.csv"), sep = ",");
# get connection data to determine the width of heatmap parts
dat <- read.csv(paste0(disease,"_connection_data.csv"), sep = ",", head = FALSE);
# Import heat connection data from SWOT clock site
heat <- read.csv(paste0(disease, "_heat_map_data.csv"), sep = ",");
heatmap <- as.matrix(t(heat[,4:12]))
width <- 1000
height <- 100
heights <- rep(height/9,9)
widths <- rep(0,length(heat[,1]))
tot <- 0
for (i in 1:length(dat[,1])){
  widths[dat[i,1]+1] <- widths[dat[i,1]+1] +1
  widths[dat[i,2]+1] <- widths[dat[i,2]+1] +1
  tot <- tot+1
}
newheat <- matrix(0,9,tot*2)
current <- 1
print(length(widths))
for (i in 1:length(widths)){
	if (widths[i]>=1)
  for (j in 1:widths[i])  {
    print(c(current, i, j))
    newheat[,current] <- heatmap[,i]
    current <- current+1
  }
}
print(tot)


colfunc <- colorRampPalette(c("blue","black", "yellow"))
breaksList = seq(-2.8284,2.8284, by = 0.05772244898)
newheat=apply(newheat, 2, rev)
pheatmap(newheat,  color = colfunc(99),cellwidth = width/length(newheat[1,]), cellheight = height/9, annotation_legend = F,
         cluster_rows=FALSE, cluster_cols=FALSE, scale = "none", legend = FALSE, border_color = NA, breaks = breaksList)

dev.off()
