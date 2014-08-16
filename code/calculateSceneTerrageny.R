## Script to calculate terrageny for each scene

# housekeeping
library(raster)
library(doMC)
library(RColorBrewer)
source("functions/removeLoss.R")
source("functions/constructTerrageny.R")

# set nCores
registerDoMC(cores=9)
scale <- 1000

# opening merged raster 
merged_amazon <- raster("../mergedTiles/forest_amazon.tif")
merged_loss <- raster("../mergedTiles/loss_amazon.tif")
# getting x and y crop sequences (for 60km*60km this is 2000px*2000px)
xDim_nPixel <- ncol(merged_amazon) - 1
xDim_seqPixel <- 1:(xDim_nPixel/scale)
xDim_seqPixel <- xDim_seqPixel*scale
yDim_nPixel <- nrow(merged_amazon) - 1
yDim_seqPixel <- 1:(yDim_nPixel/scale)
yDim_seqPixel <- yDim_seqPixel*scale
# create list of xDim extents for cropping to
extent_amazon <- bbox(merged_amazon)
extent_amazon
# getting magnitude
xMag <- extent_amazon[1,2] - extent_amazon[1,1]
xMag_perCell <- xMag/ncol(merged_amazon)
xExt_seq <- extent_amazon[1,1] + xDim_seqPixel*xMag_perCell
xExt_seq <- c(extent_amazon[1,1], xExt_seq)
xExt_seq
yMag <- extent_amazon[2,2] - extent_amazon[2,1]
yMag_perCell <- yMag/nrow(merged_amazon)
yExt_seq <- extent_amazon[2,1] + yDim_seqPixel*yMag_perCell
yExt_seq <- c(extent_amazon[2,1], yExt_seq)
yExt_seq

# upper for loop going through columns
for (y_i in 2:length(yExt_seq)) {
  # lower foreach loop going accross rows 
  foreach(x_i = 2:length(xExt_seq)) %dopar% {
    ocean <- F
    # calculate scene index
    sceneIndex <- x_i-1 + (y_i-2)*(length(yExt_seq)-1)
    # creating directory to write to
    outDir <- paste("../scenes/scene_", sceneIndex, "/", sep="")
    print(paste("calculating scene", sceneIndex))
    if(!file.exists(outDir)){
      dir.create(outDir, recursive=T)
    }
    # cropping, writing this file to directory
    hansen_forest <- crop(merged_amazon, extent(xExt_seq[x_i-1], xExt_seq[x_i], 
                                                    yExt_seq[y_i-1], yExt_seq[y_i]),
                          filename=paste(outDir, "cover_", 2000, ".tif", sep=""))
    # checking to see if scene is all ocean
    if(is.null(hansen_forest[!is.na(hansen_forest)])) {
      print(paste("scene", sceneIndex, "is all ocean"))
      unlink(outDir, recursive=T)
      ocean <- T
    }
    if (!ocean) {
      print(paste("calculating cover by year for scene", sceneIndex))
      hansen_loss <- crop(merged_loss, extent(c(xExt_seq[x_i-1], xExt_seq[x_i], 
                                                yExt_seq[y_i-1], yExt_seq[y_i])),
                          filename=paste(outDir,"loss.tif", sep=""))
      
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
                         overwrite=TRUE)
    }
  }
}
