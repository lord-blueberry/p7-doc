source("plotFunctions.r")
library(graphics)

outFolder = "./data/ppt_clean/png/"
x <- rep(0, 31)
x[11] <- 3
X <- fft(x)/length(x)
x2 <- Re(fft(X, inverse = TRUE))

plot(x, type="l",ylim=c(-0.03,3),main="Image Space")
plot(Re(X), type="l",ylim=c(-0.03,3), ylab="Real(fft(x))", main="Fourier Space")

printMap <- function(name, map,  min, max) {
  png(paste(outFolder,name, sep=""),
      width = 4.0,
      height = 4.0,
      units = "in",
      res = 254)
  print(WriteMap3(map,min, max, plotLen=1000))
  dev.off()
}

orig <- data.matrix(array(0, c(32,32)))
orig[8,12] = 2
orig[25,6] = 1
orig[15,25] = 1

model.max <- 2
model.min <- 0
model <- data.matrix(array(0, c(32,32)))
model[8,12] = 2
model[25,6] = 1
model[15,25] = 1
write.table(model, file="./data/ppt_clean/true0.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model0.png", orig-model, model.min, model.max)
model[8,12] = 1
write.table(model, file="./data/ppt_clean/true1.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model1.png", orig-model, model.min, model.max)
model[8,12] = 0.5
write.table(model, file="./data/ppt_clean/true2.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model2.png", orig-model, model.min, model.max)
model[25,6] = 0.5
write.table(model, file="./data/ppt_clean/true3.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model3.png", orig-model, model.min, model.max)
model[15,25] = 0.5
write.table(model, file="./data/ppt_clean/true4.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model4.png", orig-model, model.min, model.max)
model[8,12] = 0.1
model[25,6] = 0.1
model[15,25] = 0.1
write.table(model, file="./data/ppt_clean/true5.csv",sep=",",col.names=FALSE,row.names=FALSE)
printMap("model5.png", orig-model, model.min, model.max)


printMap("model1.png", model, model.min, model.max)
png(paste(outFolder,"model0.png",sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 256)
print(WriteMap3(psf,min(psf), max(psf), plotLen=1000))
dev.off()

psf <- data.matrix(read.table("./data/ppt_clean/psf.csv", header = FALSE, sep = ","))
png(paste(outFolder,"psf.png",sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 256)
print(WriteMap3(psf,min(psf), max(psf), plotLen=1000))
dev.off()

# call c# code
dirty0 <- data.matrix(read.table("./data/ppt_clean/dirty0.csv", header = FALSE, sep = ","))[1:32,1:32]
dMin <- min(dirty0)
dMax <- max(dirty0)
for(i in 0:5) {
  name.csv <- paste("dirty", i,".csv", sep="")
  name.png <- paste("dirty", i,".png", sep="")
  
  d <- data.matrix(read.table(paste("./data/ppt_clean/", name.csv, sep=""), header = FALSE, sep = ","))[1:32,1:32]
  printMap(name.png, d, dMin, dMax)
  
}