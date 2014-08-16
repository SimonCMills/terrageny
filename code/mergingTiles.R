## Script which opens up all downloaded cover tiles, converting to binary 
## forest-nonforest and NA for non-land (based on datamask). Then merges both 
## cover and loss tiles to large raster 

# housekeeping
library(raster)
library(doMC)
library(RColorBrewer)
source("functions/toForest.R")

# set variables (caution with number of cores to use)
threshold <- 11
registerDoMC(cores=9)

# if statement to skip forest creation if forest already has tiles in
if(length(list.files("../forestTiles/")) == 0) {
  # get cover and mask files 
  cover_names <- list.files("../downloadedTiles/treeCover/")
  cover_list <- list.files("../downloadedTiles/treeCover/", full.names=T)
  mask_list <- list.files("../downloadedTiles/dataMask/", full.names=T)
  # store list of strings which will be used to write each processed file
  write_names <- gsub("Hansen_GFC2013_treecover2000", "forest", cover_names) 
  write_names <- paste("../forestTiles/", write_names, sep="")
  
  # foreach opens up with parallel processing 
  foreach(i = 1:length(cover_list)) %dopar% {
    print(paste("calculating forest for cover", i, "of", length(cover_list)))
    cover_i <- raster(cover_list[i])
    mask_i <- raster(mask_list[i])
    forest_i <- toForest(cover_i, filename=write_names[i], type="Hansen", 
                         mask=mask_i, threshold=threshold)
    # plotting for confirmation
    pdf(paste("../pdf/merged_", i, ".pdf", sep=""))
    print(spplot(mergedTiles, 
                 colorkey=FALSE, 
                 scales=list(draw=TRUE),
                 col.regions=colorRampPalette(brewer.pal(11, "RdYlGn")[1:10]),
                 main="merged tiles", 
                 maxpixels=1e+06))
    dev.off()
  }
} else {
  print("forestTiles already exist, moving to merge")
}

# now merge these tiles into one large raster
# first, open them all
forest_list <- as.list(list.files("../forestTiles/", full.names=T))
print(forest_list)
loss_list <- as.list(list.files("../downloadedTiles/lossYear", full.names=T))
print(loss_list)
# open these as rasters (mess because raster doesn't like lists..)
for (i in 1:2) {
  loss_list[i] <- raster(as.character(loss_list[i]))
}
for (i in 1:length(forest_list)) {
  forest_list[i] <- raster(as.character(forest_list[i]))
}

write_names <- c("../mergedTiles/forest_amazon.tif", "../mergedTiles/loss_amazon.tif")
# running these two processes in parallel
foreach(i = 1:length(forest_list)) %dopar% {
  if (i == 1) {
    print("merging cover")
    toMerge_list <- forest_list
    write_name <- "../mergedTiles/forest_amazon.tif"
  }
  if (i == 2) {
    print("merging loss")
    toMerge_list <- loss_list
    write_name <- "../mergedTiles/loss_amazon.tif"
  }
  # merging and writing file
  mergedTiles <- do.call(merge, toMerge_list)
  mergedTiles <- writeRaster(mergedTiles, write_name)
  # plotting for confirmation
  pdf(paste("../pdf/merged_", i, ".pdf", sep=""))
  print(spplot(mergedTiles, 
               colorkey=FALSE, 
               scales=list(draw=TRUE),
               col.regions=colorRampPalette(brewer.pal(11, "RdYlGn")[1:10]),
               main="merged tiles", 
               maxpixels=1e+06))
  dev.off()
  print("finished")
}
