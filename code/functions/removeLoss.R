## Function.
## Removes forest by year

removeLoss <- function(cover, loss, remove.ID, filename, overwrite=FALSE) {
  coverOut <- raster(cover)
  bs <- blockSize(coverOut)
  coverOut <- writeStart(x=coverOut, filename=filename, datatype='INT2S', 
                         overwrite=overwrite)
  
  for (i in 1:bs$n) {
    valCover <- getValues(cover, row=bs$row[i], nrows=bs$nrows[i])
    valLoss <- getValues(loss, row=bs$row[i], nrows=bs$nrows[i])
    valCover[valLoss == remove.ID] <- 0
    coverOut <- writeValues(coverOut, valCover, bs$row[i])
  }
  coverOut <- writeStop(coverOut)
  return(coverOut)
}