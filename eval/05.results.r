source("plotFunctions.r")
library(graphics)

inFolder = "../../p7-cs/results/supernova/csv/"
outFolder = "../chapters/05.results/g55/"

start.name=nchar("SNR_G55_10s.128.")

writePNGs <- function(file.names, plotLen) {
  max = 0
  min = 0
  for(i in 1:length(file.names)) {
    if(!grepl(file.names[i], "raw")) {
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
    if(!grepl(file.names[i], "raw")) {
      print(WriteMap2(d,at=seq(min,max,length.out=plotLen)))
    } else {
      print(WriteMap2(d,at=seq(min(d),max(d),length.out=plotLen)))
    }
    dev.off()
  }
}

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


f <- dir(inFolder)
images <- f[endsWith(f, "image.csv")]
residuals <- f[endsWith(f, "residual.csv")]
models <- f[endsWith(f, "model.csv")]

writePNGs(images,500)
writePNGs(residuals,500)
#writePNGs(models,500)
write_stupdid(models)


calc_rms(residuals)



