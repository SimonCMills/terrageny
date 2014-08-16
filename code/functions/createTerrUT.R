## Script to raster files to be used in constructTerr.R unit tests 

createTerrUT <- function(unitTestDir, yearRange) {
  # CHECK: if out directory doesn't exist, create with warning
  if(!file.exists(unitTestDir)) {
    print("unit test directory does not exist, creating")
    dir.create(unitTestDir, recursive=TRUE)
  } 
  dir.create(paste(unitTestDir, "dynamicSingle", sep=""), showWarnings=F)
  dir.create(paste(unitTestDir, "staticSingle", sep=""), showWarnings=F)
  dir.create(paste(unitTestDir, "staticMultiple", sep=""), showWarnings=F)
  
  # Clearing out write-to directories
  sS_out <- list.files(paste(unitTestDir,"staticSingle/", sep=""), 
                         pattern=".tif", full.names=T)
  sM_out <- list.files(paste(unitTestDir,"staticMultiple/", sep=""), 
                         pattern=".tif", full.names=T)
  dS_out <- list.files(paste(unitTestDir,"dynamicSingle/", sep=""), 
                         pattern=".tif", full.names=T)
  file.remove(sS_out)
  file.remove(sM_out)
  file.remove(dS_out)

  
  # Static single through time
  staticSingle <- matrix(1, nrow=11, ncol=11)
  staticSingle <- raster(staticSingle)
  # Static multiple through time
  staticMultiple <- matrix(1, nrow=11, ncol=11)
  staticMultiple[,6] <- 0
  staticMultiple[6,] <- 0
  staticMultiple <- raster(staticMultiple)
  # looping through, creating multiple identical years 
  for (year in yearRange) {
    # writing matrices to file. 
    writeRaster(staticSingle, 
                paste(unitTestDir,"staticSingle/year_", year, ".tif", sep=""),
                overwrite=T)
    writeRaster(staticMultiple, 
                paste(unitTestDir,"staticMultiple/year_", year, ".tif", sep=""),
                overwrite=T)
  }
  # Dynamic single to multiple through time
  dynamicSingle <- matrix(1, 11, 11)
  # writing start matrix to file. 
  matrixOut <- raster(dynamicSingle)
  writeRaster(matrixOut, 
              paste(unitTestDir,"dynamicSingle/year_", yearRange[1], ".tif", sep=""),
              overwrite=T)
  # looping through, removing first columns by year, and then rows. 
  counter <- 1
  for (year in yearRange[2:length(yearRange)]) {
    if (counter <= 5) {
      dynamicSingle[,counter*2] <- 0
    } else {
      dynamicSingle[(counter-5)*2,] <- 0
    }
    # writing matrix to file.
    counter <- counter+1
    matrixOut <- raster(dynamicSingle)
    writeRaster(matrixOut, 
                paste(unitTestDir,"dynamicSingle/year_", year, ".tif", sep=""),
                overwrite=T)
    # dynamic dataset can only run up to 10 consecutive time points
    if (counter > 10) {break}
  }
}