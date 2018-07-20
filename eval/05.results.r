source("plotFunctions.r")
library(graphics)

inFolder = "../../p7-cs/results/supernova/csv/"
outFolder = "../chapters/05.results/g55/"

start.name=nchar("SNR_G55_10s.128.")

#write png images on same intensity profile except raw images
writePNGs <- function(file.names, plotLen) {
  max = 0
  min = 0
  for(i in 1:length(file.names)) {
    if(!grepl("raw", file.names[i])) {
      data <- read.table(paste(inFolder, file.names[i], sep=""), header = FALSE, sep = ",")
      d = data.matrix(data)
      max = max(max, d)
      min = min(min, d)
    }
  }
  
  for(i in 1:length(file.names)) {
    data <- read.table(paste(inFolder, file.names[i], sep=""), header = FALSE, sep = ",")
    d = data.matrix(data)
    colnames(d) = 0:(nrow(d)-1)
    rownames(d) = 0:(nrow(d)-1)
    outName <- gsub("\\.", "_", substring(basename(file.names[i]), start.name+1, nchar(basename(file.names[i]))-4))
    png(paste(outFolder,outName, ".png",sep=""),
        width = 6.0,
        height = 6.0,
        units = "in",
        res = 200)
    if(!grepl("raw", file.names[i])) {
      print(WriteMap2(d,at=seq(min,max,length.out=plotLen)))
    } else {
      print(WriteMap2(d,at=seq(min(d),max(d),length.out=plotLen)))
    }
    dev.off()
  }
}

#write pngs at a logarithmic intensity scale
write_stupdid <- function(file.names) {
  for(i in 1:length(file.names)) {
    data <- read.table(paste(inFolder, file.names[i], sep=""), header = FALSE, sep = ",")
    d = data.matrix(data)
    colnames(d) = 0:(nrow(d)-1)
    rownames(d) = 0:(nrow(d)-1)
    outName <- gsub("\\.", "_", substring(basename(file.names[i]), start.name+1, nchar(basename(file.names[i]))-4))
    png(paste(outFolder,outName, ".png",sep=""),
        width = 6.0,
        height = 6.0,
        units = "in",
        res = 200)
    min <- log(min(d) +1)
    max <- log(max(d) +1)
    at <- (exp(seq(0, log(2), length.out=400)) -1) 
    at <- at / max(at)
    at <- at * (max(d) - min(d)) + min(d)
    if(at[1] != min(d))
      at <- c(min(d), at)
    print(WriteMap2(d, at=at))
    dev.off()
  }
}

calc_rms <- function(file.names) {
  for(i in 1:length(file.names)) {
    data <- read.table(paste(inFolder, file.names[i], sep=""), header = FALSE, sep = ",")
    d = data.matrix(data)
    outName <- gsub("\\.", "_", substring(basename(file.names[i]), start.name+1, nchar(basename(file.names[i]))-4))
    
    res <- d - mean(d)
    rms <- sqrt(sum(res*res))
    print(paste(outName, rms))
  }
}

calc_line <- function(matrix, p0, p1, length.out=100) {
  a <- p1 - p0
  x0 <- p0 + a*((-p0[2]+1)/ a[2])
  y0 <- p0 + a*((-p0[1]+1)/ a[1])
  
  a <- y0 - x0
  line <-rep(c(0),length.out=length.out)
  i =1
  for(t in seq(0, 1, length.out = length.out)) {
    pt <- x0+a*t
    pt.ceil <- ceiling(pt)
    pt.floor <- floor(pt)
    pt.factor <- pt.ceil - pt 
    
    #interpolate
    xt1 <- pt.factor[1] * matrix[pt.floor[1], pt.ceil[2]] + (1-pt.factor[1]) * matrix[pt.ceil[1], pt.ceil[2]]
    xt0 <- pt.factor[1] * matrix[pt.floor[1], pt.floor[2]] + (1-pt.factor[1]) * matrix[pt.ceil[1],pt.floor[2]]
    x <- pt.factor[2] * xt0 + (1-pt.factor[2])*xt1
    line[i] <- x
    i = i+ 1
  }
  
  return(line)
}


plot_line <- function(files) {
  p0 <- c(92,29)
  p1 <- c(29,85)
  interpolation.length <- 500
  
  files1 <- c("raw", "clean", "positive_deconv" ,"L1", "L2")
  files2 <- c("raw", "clean", "TV", "starlets1", "starlets3")
  
  df1 <- data.frame(matrix(ncol=3, nrow=length(files1)*interpolation.length))
  colnames(df1) <- c("index", "value","image")
  df2 <- data.frame(matrix(ncol=3, nrow=length(files2)*interpolation.length))
  colnames(df2) <- c("index", "value","image")
  i1 <- 0
  i2 <- 0
  for(file in files) {
    data <- read.table(paste(inFolder, file, sep=""), header = FALSE, sep = ",")
    d = data.matrix(data)
    colnames(d) = 0:(nrow(d)-1)
    rownames(d) = 0:(nrow(d)-1)
    line <- calc_line(d, p0, p1, interpolation.length)
    for(f in files1) {
      if(grepl(f, file)) {
        print(file)
        start <- i1 + 1
        stop <- i1 +interpolation.length
        df1$index[start:stop] = 1:interpolation.length
        df1$value[start:stop] = line
        df1$image[start:stop] = file
        i1 = i1 + interpolation.length
        break
      }
    }
    for(f in files2) {
      if(grepl(f, file)) {
        start <- i2 + 1
        stop <- i2 +interpolation.length
        df2$index[start:stop] = 1:interpolation.length
        df2$value[start:stop] = line
        df2$image[start:stop] = file
        i2 = i2 + interpolation.length
      }
    }
  }
  
  p <- ggplot(data = df1, aes(x=df1$index, y=df1$value, colour=df1$image)) 
  p + geom_line()
  
  p <- ggplot(data = df2, aes(x=df2$index, y=df2$value, colour=df2$image)) 
  p + geom_line()
}



f <- dir(inFolder)
images <- f[endsWith(f, "image.csv")]
residuals <- f[endsWith(f, "residual.csv")]
models <- f[endsWith(f, "model.csv")]

writePNGs(images,300)
writePNGs(residuals,300)
writePNGs(models,200)


calc_rms(residuals)

library(ggplot2)
p0 <- c(92,29)
p1 <- c(29,85)
interpolation.length <- 500
df <- data.frame(matrix(ncol=3, nrow=ncol(cut)*interpolation.length))
colnames(df) <- c("index", "value","image")
i <- 0
for(image in images) {
  data <- read.table(paste(inFolder, image, sep=""), header = FALSE, sep = ",")
  d = data.matrix(data)
  colnames(d) = 0:(nrow(d)-1)
  rownames(d) = 0:(nrow(d)-1)
  line <- calc_line(d, p0, p1, interpolation.length)
  start <- i + 1
  stop <- i +interpolation.length
  df$index[start:stop] = 1:interpolation.length
  df$value[start:stop] = line
  df$image[start:stop] = image
  i = i + interpolation.length
}

p <- ggplot(data = df, aes(x=df$index, y=df$value, colour=df$image)) 
p + geom_line()
