## Function.
## Converts forest cover raster, based either on Hansen dataset (values 0:100), 
## or Isabel's datasets (values 0:3, where 1 == forest cover). Hansen dataset 
## also requires mask file to diagnose ocean tiles.

toForest <- function(cover, filename, type="", mask="", threshold="") {
  # Catching when type isn't specified.
  if (type == "") {
    stop("type needs to be specified as either Hansen or Small")
  }
  coverOut <- raster(cover)
  bs <- blockSize(coverOut)
  coverOut <- writeStart(x=coverOut, filename=filename, datatype='INT2S', 
                         overwrite=TRUE)
  
  # If dataset is Hansen, calculate cover based on mask file and threshold value
  if (type == "Hansen") {
    # Catching when type isn't specified.
    #if (mask == "" || threshold == "") {
    #  stop("for Hansen dataset a mask file and a threshold value is required")
    #}
    for (i in 1:bs$n) {
      valCover <- getValues(cover, row=bs$row[i], nrows=bs$nrows[i] )
      valMask <- getValues(mask, row=bs$row[i], nrows=bs$nrows[i] )
      valCover[valMask != 1] <- NA
      valCover[valCover < threshold] <- 0
      valCover[valCover >= threshold] <- 1
      coverOut <- writeValues(coverOut, valCover, bs$row[i])
    }
  }
  
  # If dataset is Isabel's, calculate based on MF (value == 1). 
  if (type == "Small") {
    for (i in 1:bs$n) {
      valCover <- getValues(cover, row=bs$row[i], nrows=bs$nrows[i] )
      valMask <- getValues(cover, row=bs$row[i], nrows=bs$nrows[i] )
      valCover[valCover != 1] <- 0    
      valCover[valMask == 0] <- NA
      coverOut <- writeValues(coverOut, valCover, bs$row[i])
    }
  }
  coverOut <- writeStop(coverOut)
  return(coverOut)
}