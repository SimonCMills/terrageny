### assigns all files in treeCover directory to 'coverFiles' and then computes whether or
### not forest
library(raster)
library(rgdal)
#extracting all files in directory in a list
coverFiles <- list.files("../forestTiles/treeCover", pattern="*.tif", 
                         full.names=TRUE)
#making these rasters
coverRast <- lapply(coverFiles, raster)
#now merging these
coverRast_merged <- do.call(merge, coverRast)
#writing to file
writeRaster(coverRast_merged, filename="../mergedTiles/coverMerged.tif", 
            format="GTiff", overwrite=TRUE)
