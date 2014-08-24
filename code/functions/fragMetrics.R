## Script to extract fragmentation metrics (will probably be written into 
## a function at a later date)

## Function: calculate size of fragments
fragSizes <- function(clumpedRast, clump.ids="") {
  if(clump.ids=="") {clump.ids <- 1:clumpedRast@data@max}
  require(raster)
  size.vect <- rep(NA, clumpedRast@data@max)
  bs <- blockSize(clumpedRast)
  for (i in 1:bs$n) {
    val <- getValues(clumpedRast, row=bs$row[i], nrows=bs$nrows[i])
    size <- table(val)
    size.vect.temp <- rep(NA, clumpedRast@data@max)
    size.vect.temp[as.numeric(rownames(size))] <- as.numeric(size)
    size.vect <- rowSums(cbind(size.vect, size.vect.temp), na.rm=T)  
  }
  return(size.vect)
}

## Function: nearest neighbour
nearestNeighbour <- function(clumpedRast, clump.ids="", size.vect="") {
  require(raster)
  if(clump.ids=="") {clump.ids <- 1:clumpedRast@data@max}
  if(size.vect=="") {size.vect<- fragSizes(clumpedRast)}
  
  # ignore clumps over ten% of area
  clump.ids[size.vect > ncell(clumpedRast)/10] <- NA
  #
  distance.vect <- rep(NA, clumpedRast@data@max)
  #
  for (i in clump.ids[-is.na(clump.ids)]) {
    print(i)
    cellList <- which(clumpedRast[] == i)
    # do this twice (by definition no NAs in first adjacent)
    adj <- adjacent(clumpedRast, cells=cellList, directions=8, pairs=F )
    if(any(adj %in% cellList)) {adj <- adj[-which(adj %in% cellList)]}
    adj <- adjacent(clumpedRast, cells=adj, directions=8, pairs=F)
    if(any(adj %in% cellList)) {adj <- adj[-which(adj %in% cellList)]}

    dist.counter <- 1
    while(!any(!is.na(clumpedRast[adj]))) {
      print("looping")
      dist.counter <- dist.counter + 1
      adj <- adjacent(clumpedRast, cells=adj, directions=8, pairs=F)
      if(any(adj %in% cellList)) {adj <- adj[-which(adj %in% cellList)]}
    }
    distance.vect[i] <- dist.counter
  }
  return(distance.vect)
}

# ## Function: calculate shape index of fragments 
# fragSI <- function(clumpedRast, clump.ids="", size.vect="") {
#   if(clump.ids=="") {clump.ids <- 1:clumpedRast@data@max}
#   if(size.vect=="") {size.vect<- fragSizes(clumpedRast)}
#   require(raster)
#   shape.vect <- rep(NA, clumpedRast@data@max)
#   bs <- blockSize(clumpedRast)
#   for (i in 1:bs$n) {
#     val <- getValues(clumpedRast, row=bs$row[i], nrows=bs$nrows[i])
#     shape <- table(val)
#     shape.vect.temp <- rep(NA, clumpedRast@data@max)
#     shape.vect.temp[as.numeric(rownames(size))] <- as.numeric(size)
#     size.vect <- rowSums(cbind(size.vect, size.vect.temp), na.rm=T)  
#   }
#   return(size.vect)
# }

# using SDMTools
fragSI <- function(clumpedRast) {
  require(raster)
  require(SDMTools)
  patchStats <- PatchStat(clumpedRast)
  pSI <- patchStats$shape.index
  return(pSI)
}

fracDim <- function(clumpedRast) {
  require(raster)
  require(SDMTools)
  patchStats <- PatchStat(clumpedRast)
  frac.dim <- patchStats$frac.dim.index
  return(frac.dim)
}


### Landscape stats
proportionForest <- function(clumpedRast) {
  require(raster)
  return(length(which(clumpedRast[] != 1))/ncell(clumpedRast))
}

totalEdge <- function(clumpedRast) {
  require(raster)
  patchStats <- PatchStat(clumpedRast)
  perimeter <- patchStats$perimeter
  return(sum(perimeter))
}