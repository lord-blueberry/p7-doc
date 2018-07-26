source("plotFunctions.r")
library(graphics)

start.name=nchar("SNR_G55_10s.128.")

read <- function(images) {
  resoultion <- 26
  pixels <- 128
  axis <- 0:(pixels-1) * resoultion
  images.list <- list()
  for(img in images) {
    data <- read.table(paste(inFolder, img, sep=""), header = FALSE, sep = ",")
    data = data.matrix(data)
    colnames(data) = axis
    rownames(data) = axis
    outName <- gsub("\\.", "_", substring(basename(img), start.name+1, nchar(basename(img))-4))
    images.list[[outName]] <- data
  }
  return(images.list)
}

#write png images on same intensity profile except raw images
writePNGs <- function(images.list, names, plotLen) {
  max = 0
  min = 0
  for(name in names) {
    if(!grepl("raw", name)) {
      d <- data.matrix(images.list[[name]])
      max = max(max, d)
      min = min(min, d)
    }
  }
  
  for(name in names) {
    d <- data.matrix(images.list[[name]])
    png(paste(outFolder,name, ".png",sep=""),
        width = 6.0,
        height = 6.0,
        units = "in",
        res = 200)
    if(!grepl("raw", name)) {
      print(WriteMap2(d,at=seq(min,max,length.out=plotLen)))
    } else {
      print(WriteMap2(d,at=seq(min(d),max(d),length.out=plotLen)))
    }
    dev.off()
  }
}


calcline <- function(matrix, p0, p1, length.out=100) {
  a <- p1 - p0
  x0 <- p0 + a*((-p0[2]+1)/ a[2])
  if(x0[1] > nrow(matrix)) {
    x0 <- p0 + a*((nrow(matrix)-p0[1])/ a[1])
  }
  y0 <- p0 + a*((-p0[1]+1)/ a[1])
  if(y0[2] > ncol(matrix)) {
    y0 <- p0 + a*((ncol(matrix)-p0[2])/ a[2])
  }
  
  a <- y0 - x0
  line <-rep(c(0),length.out=length.out)
  i = 1
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


calcRowLine <- function(matrix, p0,p1,length.out=100) {
  a <- p1 - p0
  x0 <- p0 + a*((-p0[2]+1)/ a[2])
  if(x0[1] > nrow(matrix)) {
    x0 <- p0 + a*((nrow(matrix)-p0[1])/ a[1])
  }
  y0 <- p0 + a*((-p0[1]+1)/ a[1])
  if(y0[2] > ncol(matrix)) {
    y0 <- p0 + a*((ncol(matrix)-p0[2])/ a[2])
  }
  
  a <- y0 - x0
  x_names <-rep(c(0),length.out=length.out)
  i = 1
  for(t in seq(0, 1, length.out = length.out)) {
    pt <- x0+a*t
    x_names[i] <- pt[1]
    i = i + 1
  }
  
  return(x_names)
}


plotline <- function(mod.list, files1, files2) {
  p0 <- c(92,29)
  #p1 <- c(29,85)
  p1 <- c(46,59)
  interpolation.length <- 500
  
  df1 <- data.frame(matrix(ncol=3, nrow=length(files1)*interpolation.length))
  colnames(df1) <- c("index", "value","image")
  i1 <- 0
  for (file in files1) {
    d = data.matrix(mod.list[[file]])
    print(file)
    line <- rev(calcline(d, p0, p1, interpolation.length))
    start <- i1 + 1
    stop <- i1 + interpolation.length
    df1$index[start:stop] = 1:interpolation.length
    df1$value[start:stop] = line
    df1$image[start:stop] = file
    i1 = i1 + interpolation.length
  }
  print("")
  df2 <- data.frame(matrix(ncol=3, nrow=length(files2)*interpolation.length))
  colnames(df2) <- c("index", "value","image")
  i2 <- 0
  for (file in files2) {
    print(file)
    d = data.matrix(mod.list[[file]])
    line <- rev(calcline(d, p0, p1, interpolation.length))
    start <- i2 + 1
    stop <- i2 +interpolation.length
    df2$index[start:stop] = 1:interpolation.length
    df2$value[start:stop] = line
    df2$image[start:stop] = file
    i2 = i2 + interpolation.length
  }
  rowNames <- calcRowLine(data.matrix(mod.list[[1]]), p0,p1, interpolation.length)
  df1$index <- rev(rep(rowNames,length(files1)))
  df2$index <- rev(rep(rowNames,length(files2)))
  print(ggplot(data = df1, aes(x=df1$index, y=df1$value, colour=df1$image)) + geom_line())
  print(ggplot(data = df2, aes(x=df2$index, y=df2$value, colour=df2$image)) + geom_line())
}


writeExport <- function(mod.list, files1, files2) {
  p0 <- c(92,29)
  #p1 <- c(29,85)
  p1 <- c(46,59)
  interpolation.length <- 500
  
  df1 <- data.frame(matrix(ncol=length(files1), nrow=interpolation.length))
  colnames(df1) <- files1
  row.names <- rev(calcRowLine(data.matrix(mod.list[["raw_model"]]), p0, p1, interpolation.length))
  rownames(df1) <- row.names
  for (file in files1) {
    print(file)
    d = data.matrix(mod.list[[file]])
    line  <- rev(calcline(d, p0, p1, interpolation.length))
    df1[1:interpolation.length, file] = line
  }
  
  df2 <- data.frame(matrix(ncol=length(files2), nrow=interpolation.length))
  colnames(df2) <- files2
  rownames(df2) <- row.names
  for (file in files2) {
    print(file)
    d = data.matrix(mod.list[[file]])
    line <- rev(calcline(d, p0, p1, interpolation.length))
    df2[1:interpolation.length, file] = line
  }
  
  write.table(df1, file="./data/part1.csv", sep=";")
  write.table(df2, file="./data/part2.csv", sep=";")
}

inFolder = "../../p7-cs/results/supernova/csv/"
outFolder = "../chapters/05.results/g55/"
f <- dir(inFolder)
images <- f[endsWith(f, "image.csv")]
residuals <- f[endsWith(f, "residual.csv")]
models <- f[endsWith(f, "model.csv")]

img.list <- read(images)
img.names <- gsub("\\.", "_", substring(basename(images), start.name+1, nchar(basename(images))-4))
res.list <- read(residuals)
res.names <- gsub("\\.", "_", substring(basename(residuals), start.name+1, nchar(basename(residuals))-4))
mod.list <- read(models)
mod.names <- gsub("\\.", "_", substring(basename(models), start.name+1, nchar(basename(models))-4))

#modify clean
clean.img <-  data.matrix(img.list[["clean_image"]])
clean.res <-  data.matrix(res.list[["clean_residual"]])
diff <- clean.img - clean.res
mod.list[["clean_model"]] <- diff
mod.list[["raw_model"]] <- data.matrix(img.list[["raw_image"]])

#writePNGs(img.list, img.names, 300)
#writePNGs(res.list,res.names, 300)
writePNGs(mod.list,mod.names, 200)

files1 <- c("raw_model", "clean_model", "positive_deconv_model" ,"L1_model", "L2_model")
files2 <- c( "clean_model", "L1+L2_model","TV_model", "starlets3_model")
#writeExport(mod.list, files1, files2)

library(ggplot2)
library(scales)

plotline(mod.list,files1, files2)

files1 <- c("raw_image", "clean_image", "positive_deconv_image", "L1_image", "L2_image")
files2 <- c( "clean_image", "L1+L2_image","TV_image", "starlets3_image")
plotline(img.list,files1, files2)
