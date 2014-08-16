## Script to calculate terrageny for each scene

# housekeeping
library(raster)
library(doMC)
library(RColorBrewer)
source("functions/removeLoss.R")
source("functions/constructTerrageny_ver2.R")
rasterOptions(datatype="INT2S")

# set nCores
registerDoMC(cores=10)
scale <- 2000

# opening hansen
hansen_tile <- raster("../forestTiles/forest_00N_060W.tif")
spplot(hansen_tile, scales=list(draw=T))
hansen_loss <- raster("../downloadedTiles/lossYear/Hansen_GFC2013_lossyear_00N_060W.tif")
# getting x and y crop sequences (for 60km*60km this is 2000px*2000px)
xDim <- ncol(hansen_tile) - 1
xEdges <- seq(scale, xDim, scale)
yDim <- nrow(hansen_tile) - 1
yEdges <- seq(scale, yDim, scale)
# create list of xDim extents for cropping to
hansen_extent <- bbox(hansen_tile)
# getting magnitude
xEdges <- hansen_extent[1,1] + xEdges*res(hansen_tile)[1]
yEdges <- hansen_extent[2,1] + yEdges*res(hansen_tile)[2]
xEdges <- c(hansen_extent[1,1], xEdges)
yEdges <- c(hansen_extent[2,1], yEdges)

edgeMatrix <- matrix(nrow=81, ncol=4, dimnames=list(1:81, c("xMin", "xMax", 
                                                            "yMin", "yMax")))
counter <- 1
for (x_i in seq(1, length(xEdges)-1, 2)) {
  # lower foreach loop going accross rows 
  for (y_i  in seq(1, length(yEdges)-1, 2)) {
    edgeMatrix[counter,] <- c(xEdges[x_i], xEdges[x_i+1], 
                         yEdges[y_i], yEdges[y_i+1])
    counter <- counter+1
  }
}
print(edgeMatrix)

foreach (scene_i = 1:length(edgeMatrix[,1])) %dopar% {
  # creating directory to write to
  outDir <- paste("../scenes_tile2/scene_", scene_i, "/", sep="")
  print(paste("calculating scene", scene_i))
  # create directory if doesn't exist already
  if (!file.exists(outDir)) dir.create(outDir, recursive=T)
  # cropping, writing this file to directory
  hansen_forest <- crop(hansen_tile, extent(as.numeric(edgeMatrix[scene_i,])),
                        filename=paste(outDir, "cover_", 2000, ".tif", sep=""))
  # checking to see if scene is all ocean
  #if(is.null(hansen_forest[!is.na(hansen_forest)])) {
  #  print(paste("scene", sceneIndex, "is all ocean"))
  #  unlink(outDir, recursive=T)
  #  ocean <- T
  #}
  #if (!ocean) {
    print(paste("calculating cover by year for scene", scene_i))
    hansen_loss <- crop(hansen_loss, extent(as.numeric(edgeMatrix[scene_i,])),
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
  #}
}


