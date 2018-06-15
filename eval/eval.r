#library(lattice)
#levelplot(data.matrix(data, rownames.force = NA), xlab="", ylab="")

#library(pheatmap)
#pheatmap(data.matrix(data, rownames.force = NA), cluster_row=FALSE, cluster_col=FALSE)




#images
data <- read.table("./data/01.intro/sun.flare00_64.64/dirty.csv", header = FALSE, sep = ",")
library(rgdal)
library(rasterVis)
library(viridis)

d = data.matrix(data)
colnames(d) = 0:63
rownames(d) = 0:63
oldmargin = par()$mar

matrix = d
par(mar=c(0,0,0,0),plt=c(0,0,0,0),oma=c(0,0,0,0))
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





data <- read.table("./data/01.intro/sun.flare00_64.64/dirty.csv", header = FALSE, sep = ",")
d = data.matrix(data)
colnames(d) = 0:63
rownames(d) = 0:63

WriteMap <- function(d) {
  #lattice.options(axis.padding=list(factor=0.5))
  par(mar=c(0,0,0,0),plt=c(0,0,0,0),oma=c(0,0,0,0))
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
}

png("mytest.png",
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
WriteMap(d)
dev.off()


Convolve <- function(x, kernel) {
  k <- apply(kernel, 2, rev)
  k <- apply(k, 1, rev)
  centerY <- ncol(kernel)/2 
  centerX <- nrow(kernel)/2
  output <- matrix(0, ncol=ncol(x),nrow=nrow(x))
  
  for(col in 1:ncol(x)) {
    for (row in 1:nrow(x)) {
      sum = 0
      for(psfY in 1:ncol(kernel)) {
        for (psfX in 1:nrow(kernel)) {
          Y <- col - centerY + psfY
          X <- row - centerX + psfX
          if(X >= 1 & X <= nrow(x) & Y >=1 & Y <= ncol(x)) {
            sum = sum + x[Y,X] * k[psfY,psfX]
          }
        }
      }
      output[col,row] = sum
    }
  }
  return(output)
}


a <- matrix(c(1:4,1:4,4:1,4:1),ncol=4,nrow=4)
b <- matrix(1,ncol=4,nrow=4)
b[3,3] = 2
c <- Convolve(a,b)



UVPlot <- function(u,v) {
  par(mar=c(0,0,0,0))
  plot(u,v, axes=F,xlab="U",ylab="V", pch=15)
  axis(1, pos=0, cex.axis=0.8)
  axis(2, pos=0, cex.axis=0.8)
}


vis <- read.csv("./data/01.intro/listvis.csv", header = FALSE, sep = ";",as.is=T)
png("mytest.png",
    width = 12.0,
    height = 12.0,
    units = "in",
    res = 200)
UVPlot(vis$V11,vis$V12)
dev.off()



