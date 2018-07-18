source("plotFunctions.r")
library(graphics)

inImgFolder = "../../p7-cs/casa-cs/analysis_pipeline/csv_img/"
outFolder = "../chapters/01.intro/img/"


vis <- read.csv("./data/01.intro/listvis.csv", header = FALSE, sep = ";",as.is=T)
png(paste(outFolder,"uv.png",sep=""),
    width = 12.0,
    height = 12.0,
    units = "in",
    res = 200)
UVPlot(vis$V11,vis$V12)
dev.off()

data <- read.table(paste(inImgFolder,"sun.flare00_256.256/psf.csv",sep=""), header = FALSE, sep = ",")
d = data.matrix(data)
colnames(d) = 0:(nrow(d)-1)
rownames(d) = 0:(nrow(d)-1)
png(paste(outFolder,"psf.png",sep=""),
    width = 6.0,
    height = 6.0,
    units = "in",
    res = 200)
WriteMap(d,plotLen=1000)
dev.off()


true_image <- Write_TrueImage(count=15)
png(paste(outFolder,"true_image.png",sep=""),
    width = 6.0,
    height = 6.0,
    units = "in",
    res = 200)
WriteMap(true_image)
dev.off()


write.table(true_image, file="./data/true_image.csv",sep=",",col.names=FALSE,row.names=FALSE)


dirty <- data.matrix(read.table("./data/dirty_image.csv", sep=",", header=FALSE))[1:128,1:128]
png(paste(outFolder,"dirty_image.png",sep=""),
    width = 6.0,
    height = 6.0,
    units = "in",
    res = 1000)
WriteMap(dirty,plotLen=200)
dev.off()

dirty[89,21]
dirty[34,64]

