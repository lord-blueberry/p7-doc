library(rgdal)
library(rasterVis)
library(viridis)


WriteMap <- function(x,plotLen=100) {
  #lattice.options(axis.padding=list(factor=0.5))
  par(mar=c(0,0,0,0),plt=c(0,0,0,0),oma=c(0,0,0,0))
  levelplot(x,
            margin=FALSE,
            par.settings=list(
              axis.line=list(col='transparent') # suppress axes and legend outline
            ),
            scales=list(draw=FALSE),
            col.regions=viridis,
            colorkey = FALSE,
            at=seq(min(x),max(x),len=plotLen),
            xlab="",
            ylab=""
            
  )
}


WriteMap2 <- function(x, at) {
  par(mar=c(0,0,0,0),plt=c(0,0,0,0),oma=c(0,0,0,0))
  levelplot(x,
            at = at,
            margin=FALSE,
            par.settings=list(
              axis.line=list(col='transparent') # suppress axes and legend outline
            ),
            scales=list(draw=FALSE),
            col.regions=viridis,
            colorkey = FALSE,
            xlab="",
            ylab=""
            
  )
}


conv_slow <- function(x, kernel) {
  k <- apply(kernel, 2, rev)
  k <- apply(k, 1, rev)
  centerY <- ncol(kernel)/2 
  centerX <- nrow(kernel)/2
  output <- matrix(0, ncol=ncol(x),nrow=nrow(x))
  start_time <- Sys.time()
  for(row in 1:nrow(x)) {
    for (col in 1:ncol(x)) {
      sum = 0
      for(psfX in 1:nrow(kernel)) {
        for (psfY in 1:ncol(kernel)) {
          Y <- col - centerY + psfY
          X <- row - centerX + psfX
          if(X >= 1 & X <= nrow(x) & Y >=1 & Y <= ncol(x)) {
            sum = sum + x[Y,X] * k[psfY,psfX]
          }
        }
      }
      output[col,row] = sum
    }
    print(paste("done row",row))
    print(Sys.time()-start_time)
  }
  end <- Sys.time()
  print(end-start_time)
  return(output)
}


UVPlot <- function(u,v) {
  par(mar=c(0,0,0,0))
  plot(u,v, axes=F,xlab="U",ylab="V", pch=15)
  axis(1, pos=0, cex.axis=0.8)
  axis(2, pos=0, cex.axis=0.8)
}


Write_TrueImage <- function(count=25) {
  set.seed(123)
  size = 128
  true_image <- matrix(0,ncol=size,nrow=size)
  true_image[33,64]= 7
  true_image[33,63]= 7
  true_image[34,63]= 7
  true_image[34,64]= 6
  
  true_image[35,62]= 1
  true_image[35,63]= 3
  true_image[35,64]= 3
  true_image[35,65]= 1
  true_image[32,62]= 1
  true_image[32,63]= 2
  true_image[32,64]= 2
  true_image[32,65]= 1
  
  true_image[33,65]= 2
  true_image[34,65]= 5
  true_image[33,62]= 2
  true_image[34,62]= 1
  
  true_image[89,22]= 8
  true_image[88,21]= 8
  true_image[89,21]= 8
  true_image[88,22]= 8
  
  randIdx = sample(1:size, count*2)
  randVal = runif(count, min=0.01, max=1.0)
  for (i in 1:count) {
    true_image[randIdx[i], randIdx[count+i]] = randVal[i]
  }
  
  return(true_image)
}