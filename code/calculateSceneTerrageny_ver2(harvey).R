## Rscript to calculate scenes for analysis 

# housekeeping
rm(list=ls())
graphics.off()
library(raster)
library(doMC)
library(RColorBrewer)
source("functions/removeLoss.R")
source("functions/constructTerrageny_ver2.R")
rasterOptions(datatype="INT2S")


# set nCores
registerDoMC(cores=10)
scale <- 2000
cropExtent <- c(-74, -40, -19, 7)
sampling <- T

# opening mergedAmazon file
mergedAmazon <- raster("../mergedTiles/forest_amazon.tif")
lossAmazon <- raster("../mergedTiles/loss_amazon.tif")

# opening legalAmazon mask, setting projection, and cropping
legalAmazon <- raster("../legalAmazon/8kmlegamazon.dat")
projection(legalAmazon) <- projection(mergedAmazon)
legalAmazon <- crop(legalAmazon, extent(cropExtent))

# getting x and y crop sequences (for 60km*60km this is 2000px*2000px)
xEdges <- seq(scale, scale*60, scale)
yEdges <- seq(scale, scale*46, scale)
legAm_matrix <- matrix(NA, ncol=60, nrow=46)
nCells <- dim(legAm_matrix)[1]*dim(legAm_matrix)[2]
refMatrix <- matrix(1:nCells, nrow=dim(legAm_matrix)[1], ncol=dim(legAm_matrix)[2])
edgeMatrix <- matrix(nrow=nCells, ncol=4, dimnames=list(1:nCells, c("xMin", "xMax", 
                                                                    "yMin", "yMax")))
# getting magnitude
xEdges <- cropExtent[1] + xEdges*res(mergedAmazon)[1]
yEdges <- cropExtent[3] + yEdges*res(mergedAmazon)[2]
xEdges <- c(cropExtent[1], xEdges)
yEdges <- c(cropExtent[3], yEdges)

sequence <- c()
counter <- 1
# loop over x and y, checking if in legalAmazon, entering this into matrix
for (x_i in seq(1, length(xEdges)-1, 1)) {
  # lower foreach loop going accross rows 
  for (y_i  in seq(1, length(yEdges)-1, 1)) {
    edgeMatrix[counter,] <- c(xEdges[x_i], xEdges[x_i+1], 
                             yEdges[y_i], yEdges[y_i+1])
    
    legAm_i <- crop(legalAmazon, extent(c(xEdges[x_i], xEdges[x_i+1], 
                                          yEdges[y_i], yEdges[y_i+1])))
    if (length(which(legAm_i[] == 1)) == 0) {
      legAm_matrix[y_i, x_i] <- 0
    } else {
      legAm_matrix[y_i, x_i] <- 1
      sequence <- c(sequence, refMatrix[y_i, x_i])
    }
    counter <- counter+1
  }
}
print(paste("scenes:", length(sequence)))

# "sequence" is full sequence. Initially only sampling over this. 
if (sampling) {
  print("sampling is true; calculating sample")
  for(col_i in seq(2, ncol(refMatrix), 2)) {
    for (i in 1:length(refMatrix[,col_i])) {
      if (length(which(sequence == refMatrix[i,col_i])) != 0) {
        sequence <- sequence[-which(sequence == refMatrix[i,col_i])]
      }
    } 
  }
  # remove even numbers
  sequence <- sequence[-which(sequence %% 2 == 0)]
  # choosing the second half of this to be analysed first 
  sequence <- sequence[1:which(sequence==1385)]
  print(paste("sampled scenes:", length(sequence)))
}

# now going through scenes (in parallel), and calculating terrageny
foreach (scene_i = sequence) %dopar% {
  # creating directory to write to
  outDir <- paste("../scenes/scene_", scene_i, "/", sep="")
  print(paste("calculating scene", scene_i))
  # create directory if doesn't exist already
  if (!file.exists(outDir)) dir.create(outDir, recursive=T)
  # cropping, writing this file to directory
  hansen_forest <- crop(mergedAmazon, extent(as.numeric(edgeMatrix[scene_i,])),
                        filename=paste(outDir, "cover_", 2000, ".tif", sep=""))
  
  print(paste("calculating cover by year for scene", scene_i))
  hansen_loss <- crop(lossAmazon, extent(as.numeric(edgeMatrix[scene_i,])),
                      filename=paste(outDir,"loss.tif", sep=""))
  
  pdf(paste(outDir, "loss.pdf", sep=""))
  print(spplot(hansen_loss,
               maxpixels=10000,
               col.regions=colorRampPalette(brewer.pal(9, "Greens")[c(2,7)]),
               scales=list(draw=TRUE)))
  dev.off()
  
  # removing loss by year, writing each file to directory
  for (year in 1:12) {
    lossName <- paste(outDir, "cover_", year+2000, ".tif", sep="" )
    hansen_forest <- removeLoss(hansen_forest, hansen_loss, remove.ID=year, 
                                filename=lossName) 
  }
  # now construct terrageny 
  print("now constructing terrageny")
  constructTerrageny(inPath=outDir, 
                     outPath=paste(outDir, "clumped/", sep=""), 
                     inFileStart="cover_",
                     fileType=".tif", 
                     overwrite=TRUE,
                     verbose=F)
}