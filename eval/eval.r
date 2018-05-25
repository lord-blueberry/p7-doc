data <- read.table("./data/01.intro/sun.flare00_64.64/dirty.csv", header = FALSE, sep = ",")

#library(lattice)
#levelplot(data.matrix(data, rownames.force = NA), xlab="", ylab="")

library(pheatmap)
pheatmap(data.matrix(data, rownames.force = NA), cluster_row=FALSE, cluster_col=FALSE)

library(rgdal)
library(rasterVis)
library(viridis)

d = data.matrix(data)
colnames(d) = 0:63
rownames(d) = 0:63
oldmargin = par()$mar

png("mytest.png")
v <- 0.1
lattice.options(axis.padding=list(factor=0.5))
par(mar=c(0,0,0,0),plt=c(0,0,0,0),oma=c(0,0,0,0),cex.lab=v, cex=v,cex.axis=v, cex.main=v, cex.sub=v)
levelplot(d,
          margin=FALSE,
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),
          col.regions=viridis,
          colorkey = FALSE,
          at=seq(min(d),max(d),len=100),
          xlab="",
          ylab=""
          
          )
dev.off()