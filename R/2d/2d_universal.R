library(circlize)
library(MASS)
library(png)
#lines for autism
lines=c("                                      Log Odds         p-Value     ",
        "Pluripotency                  -0.9072             0.0004      ",
        "Neuroectoderm             -1.2220             0.0001      ",
        "Neural Differentiation     0.2866             0.2654      ",
        "Cortical Specification     0.5541             0.0178      ",
        "Deep Layers                  0.2306             0.3420      ",
        "Upper Layers                 0.6258             0.0021      ")
#lines for microcephaly
#lines=c("                                      Log Odds         p-Value     ",
#        "Pluripotency                  -0.2051             0.4736      ",
#        "Neuroectoderm              1.0900             0.0000      ",
#        "Neural Differentiation    -0.4957             0.2577      ",
#        "Cortical Specification     0.1204             0.6492      ",
#        "Deep Layers                 -0.4066             0.3426      ",
#        "Upper Layers                -1.0800             0.0023      ")
#lines for antisocial
#lines=c("                                      Log Odds         p-Value     ",
#        "Pluripotency                   0.4842             0.3041      ",
#        "Neuroectoderm             -1.4660             0.1532      ",
#        "Neural Differentiation     0.9257             0.0754      ",
#        "Cortical Specification    -1.8500             0.0982      ",
#        "Deep Layers                 -0.2289             1.0000      ",
#        "Upper Layers                 0.3410             0.5670      ")
#for autism
#strange_argument=c(15,8,19,27,19,39)
#for microcephaly
#strange_argument=c(45, 74, 19, 20, 12, 14)
#for antisocial
#strange_argument=c(6,2,5,2,5,0)
disease_name="Atypical_autism"
disease_name_with_space="Atypical autism"
selected_phase=0
png_name=paste0(paste0(disease_name,toString(selected_phase)),"_2d.png")
png(filename = png_name,
    type = "cairo",
    units="in",
    width = 30,
    height = 30,
    pointsize = 1,
    res = 150)

# import connection data from csv
dat <- read.csv(paste0(disease_name,"_connection_data.csv"), sep = ",", head = FALSE);
# import heat connection data from csv
heat <- read.csv(paste0(disease_name,"_heat_map_data.csv"), sep = ",");

# heatmap data extraction & formatting
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
widths[1]=widths[1]
newheat <- matrix(0,9,tot*2)
for (i in 1:length(dat[,1])){
	if (isTRUE(all.equal(widths[i], 0)))
		widths[i]<-2
}

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
newheat=apply(newheat, 2, rev)

print(tot)


# chrodiagram data extraction & formatting
lab <- heat[,2]

tmp <- as.matrix(dat);
tmp = tmp +1
matt <- matrix(0,length(lab),length(lab))
matt[tmp] <- 1
rownames(matt) = lab
colnames(matt) = lab
cluster <- heat[,3]
name <- as.character(heat[,2])


# set background color of canvas
par(bg = 'black')

# heatmap ring
# generate dendrograms for all clusters
col_fun = colorRamp2(c(-2.8284,0,2.8284), c("blue","black", "yellow"))
ori_factors = rep(letters[1:6], times = c(1,0,3,3,1,2))
factors<-c()
for (i in 1:length(ori_factors)) {
	for (j in 1:widths[i]) {
		factors=c(factors,ori_factors[i])
	}
}
row.names(newheat)<-row.names(heatmap)
mat_list = list(a = newheat[, factors == "a"],
                b = newheat[, factors == "b"],
                c = newheat[, factors == "c"],
                d = newheat[, factors == "d"],
                e = newheat[, factors == "e"],
                f = newheat[, factors == "f"])
#dend_list = list(a = as.dendrogram(hclust(dist(t(mat_list[["a"]])))),
#                 b = as.dendrogram(hclust(dist(t(mat_list[["b"]])))),
#                 c = as.dendrogram(hclust(dist(t(mat_list[["c"]])))),
#                 d = as.dendrogram(hclust(dist(t(mat_list[["d"]])))),
#                 e = as.dendrogram(hclust(dist(t(mat_list[["e"]])))))

# setting circos parameter and generating heatmap
circos.par(cell.padding=c(0,0,0,0), gap.degree=0, start.degree=90, clock.wise=TRUE)
circos.initialize(factors, xlim = cbind(c(0, 0), table(factors)))
circos.track(ylim = c(0, 9), bg.border = NA, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  m = mat_list[[sector.index]]

  #dend = dend_list[[sector.index]]
  
  col_mat = col_fun(m)
  nr = nrow(m)
  nc = ncol(m)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc),
                (1):(nc), rep(nr - i + 1, nc),
                border = col_mat[i, ], col = col_mat[i, ])
  }
})

# clear circos because we are generating chrodiagram then combine both on a canvas
circos.clear()

# chordiagram
# setting color scheme for chordiagram
colors = c(rgb(215, 48, 39, maxColorValue=255), rgb(252, 141, 89, maxColorValue=255),rgb(254, 224, 144, maxColorValue=255), rgb(224, 243, 248, maxColorValue=255), rgb(145, 191, 219, maxColorValue=255), rgb(69, 117, 180, maxColorValue=255))
grid.col = c()
length(grid.col) <- length(lab)
for (i in 1:length(lab)){
  grid.col[i] <- assign(name[i], colors[cluster[i]])
}

# one line of magic that combines both visualizations
par(new=TRUE)

# setting new parameter
circos.par("canvas.xlim" = c(-1.25, 1.25), "canvas.ylim" = c(-1.25, 1.25), 
           gap.degree = 0, start.degree = 90, clock.wise = TRUE)

# generate chordiagram
chordDiagram(t(matt),grid.col = grid.col, transparency = 0,
             self.link = 2, annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(matt))))),
             link.lwd = .01, link.border = "black")

#legend(0.03, 0.025,
#       c("Male", "Female"),
#       col = c("blue", "deeppink"),
#       cex = 0.8,
#       lwd = 1, lty = 1)
#legend(0.03, 0.015,
#       c("Blue species", "Orange species"),
#       cex = 0.8,
#       pch = c(1,2))

dev.off()
circos.clear()
#png(filename = "Rplot%03d.png", width = 480, height = 480,
#    units = "px", pointsize = 12, bg = "white", res = NA,
#    restoreConsole = TRUE)


img<-readPNG(png_name)

#get size
h<-dim(img)[1]
w<-dim(img)[2]

#open new file for output
png(png_name, width=w*2, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F,bg = 'black')

plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")    
rasterImage(img, usr[1], usr[3], (usr[1]+usr[2])/2, usr[4])
xmin <- par("usr")[1]
xmax <- par("usr")[2]
ymin <- par("usr")[3]
ymax <- par("usr")[4]


#add text
text(xmin+0.52*(xmax-xmin),ymin+0.70*(ymax-ymin), disease_name_with_space, cex=20, col=rgb(1,1,1,1),pos=4)
for (i in 1:7) {
  text(xmin+0.52*(xmax-xmin),ymin+(0.70-i*0.05)*(ymax-ymin), lines[i], cex=15, col=rgb(1,1,1,1),pos=4)
}
if (selected_phase!=0) {
text(xmin+0.50*(xmax-xmin),ymin+(0.70-(selected_phase+1)*0.05)*(ymax-ymin), "✖", cex=15, col=colors[selected_phase],pos=4)
}

#Now determine the size of the legend you would like to plot.  Right now the exact
#location is not important, we just want to know the dimension!  Note that we are
# treating the lengend as a variable and we are NOT plotting the legend on the figure!
# Add legend in the lower right corner:
lgd=legend(x = xmin+0.45*(xmax-xmin), y =  ymax, inset=0, 
           c("-2.8284"," 0"," 2.8284"), fill=c("blue","black", "yellow"), horiz=FALSE, cex=10,col = c("white"),text.col= c("white"),y.intersp=0.8,plot = F,bg="black",border="white")
legend(x = xmin+0.45*(xmax-xmin), y =  ymin+lgd$rect$h, inset=0, 
           c("-2.8284"," 0"," 2.8284"), fill=c("blue","black", "yellow"), horiz=FALSE, cex=10,col = c("white"),text.col= c("white"),y.intersp=0.8,plot = T,bg="black",border="white")
legend(x = xmin+0.45*(xmax-xmin), y =  ymax, inset=0, 
       c("Pluripotency","Neuroectoderm","Neural Differentiation","Cortical Specification","Deep Layers","Upper Layers"), fill=colors, horiz=FALSE, cex=10,col = c("white"),text.col= c("white"))
dev.off()
