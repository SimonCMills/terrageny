## Constructs terrageny (edited from Dan's code)
## Data provided in raster format (set by fileType variable)

constructTerrageny <- function(inPath, outPath, inFileStart, fileType, 
                               overwrite=F, verbose=T) {
  # CHECK: attach package dependencies, or if not installed stop with warning
  if(!require(raster)) {
    stop ("require raster package to be installed")
  } 
  if(!require(gridExtra)) {
    stop ("require gridExtra package to be installed")
  } 
  if(!require(RColorBrewer)) {
    stop ("require RColorBrewer package to be installed")
  }
  
  # CHECK: if in directory doesn't exist, stop
  if(!file.exists(inPath)) {
    stop("inPath does not exist")
  }
  # CHECK: if out directory doesn't exist, create with warning
  if(!file.exists(outPath)) {
    print("outPath does not exist, creating")
    dir.create(outPath, recursive=TRUE)
  }
  
  # dataset specific variables, to be altered for varying datasets (slightly 
  # superceded now wrapped in function)
  outClumpStart <- gsub(inFileStart, "clumpedYear_", inFileStart)
  outFileStart <- "cover_"
  setRes <- 10000
  
  # Getting all.yrs from file, breaking from run if no files provided
  all.yrs <- list.files(inPath, pattern=fileType)
  if (verbose) print(paste("all years:", all.yrs))
  
  # CHECK: are there files in supplied dir?
  if (length(all.yrs) == 0) {stop("no files found in inPath directory")}
  all.yrs <- gsub(pattern=inFileStart, replacement="", all.yrs)
  all.yrs <- gsub(pattern=fileType, replacement="", all.yrs)
  all.yrs <- as.numeric(all.yrs)
  all.yrs <- sort(all.yrs, decreasing=TRUE)
  
  # CHECK: Checking write directory for pre-existing files
  outFiles <- list.files(outPath, full.names=T)
  if (length(outFiles)!=0) {
    if (overwrite == TRUE) {
      file.remove(outFiles)
      if (verbose) print("outPath directory overwritten")
    } else {
      stop("outPath directory has files, set overwrite=TRUE to overwrite")
    }   
  }
  
  ## Terrageny construction 
  ## CONSTRUCTING TERRA.1
  #initialize results storage receptacle
  terra.1<-data.frame(t=NA*numeric(1),
                    within.t.ID=NA*numeric(1),
                    size=NA*numeric(1),
                    ancestor.t=NA*numeric(1),
                    ancestor.within.t.ID=NA*numeric(1))
  
  #load the map for the first time step
  t.counter<-length(all.yrs)
  if (verbose) print(paste('Year counter:', all.yrs[t.counter]))
  inName<-paste(inPath, inFileStart, all.yrs[t.counter], fileType, sep="")
  outName<-paste(outPath, outFileStart, all.yrs[t.counter], fileType, sep="")
  d<-raster(inName)
  
  
  #find the chunks for the first year and save
  clumpName<-paste(outPath, outClumpStart, all.yrs[t.counter], fileType, sep='')
  dc<-clump(d, filename=clumpName)
  maxClumps <- as.numeric(dc@data@max)
  nClumps<-1:maxClumps
  
  #opening document to plot to
  pdf(file=paste(outPath, 'cover&clump.pdf',sep=''))
  # plotting to pdf
  coverYear_plot <- spplot(d, 
                           main=paste("Cover; year: ", all.yrs[t.counter], sep=""), 
                           maxpixels=setRes,
                           col.regions=colorRampPalette(brewer.pal(9, "Greens")[c(2,7)]),
                           scales=list(draw=TRUE),
                           colorkey=FALSE)
  clumpYear_plot <- spplot(dc, 
                           main=paste("Clumped; nClumps:", maxClumps), 
                           maxpixels=setRes,
                           col.regions=colorRampPalette(brewer.pal(11, "RdYlGn")), 
                           scales=list(draw=TRUE),
                           colorkey=FALSE)
  grid.arrange(coverYear_plot, clumpYear_plot, ncol=2)
  
  #put information about each chunk in the results storage receptacle
  for (c.counter in 1:length(nClumps))
  {
    terra.1[c.counter,'t']<-all.yrs[t.counter]
    terra.1[c.counter,'within.t.ID']<-c.counter
    #terra.1[c.counter,'size']<-length(which(dc[]==c.counter))
    terra.1[c.counter,'ancestor.t']<-NA
    terra.1[c.counter,'ancestor.within.t.ID']<-NA
  }
  for (t.counter in (length(all.yrs)-1):1)
  {
    if (verbose) print(paste('Year counter:', all.yrs[t.counter]))
    
    #load the map for this time step
    inName<-paste(inPath, inFileStart, all.yrs[t.counter], fileType, sep="")
    outName<-paste(outPath, outFileStart, all.yrs[t.counter], sep="")
    d<-raster(inName)
  
    #find the chunks and save
    clumpName<-paste(outPath, outClumpStart, all.yrs[t.counter], fileType, sep='')
    dc.old<-dc
    dc<-clump(d, filename=clumpName)
    maxClumps <- as.numeric(dc@data@max)
    nClumps<-1:maxClumps
    
    # plotting to pdf
    coverYear_plot <- spplot(d, 
                             main=paste("Cover; year: ", all.yrs[t.counter], sep=""), 
                             maxpixels=setRes,
                             col.regions=colorRampPalette(brewer.pal(9, "Greens")[c(2,7)]),
                             scales=list(draw=TRUE),
                             colorkey=FALSE)
    clumpYear_plot <- spplot(dc, 
                             main=paste("Clumped; nClumps:", maxClumps), 
                             maxpixels=setRes,
                             col.regions=colorRampPalette(brewer.pal(11, "RdYlGn")), 
                             scales=list(draw=TRUE),
                             colorkey=FALSE)
    grid.arrange(coverYear_plot, clumpYear_plot, ncol=2)
    
    #put information about each chunk in the results storage receptacle
    positions<-1:length(nClumps)+dim(terra.1)[1]
    for (c.counter in 1:length(nClumps))
    {
      terra.1[positions[c.counter],'t']<-all.yrs[t.counter]
      terra.1[positions[c.counter],'within.t.ID']<-c.counter
      #terra.1[positions[c.counter],'size']<-length(which(dc[]==c.counter))
      terra.1[positions[c.counter],'ancestor.t']<-all.yrs[t.counter+1]
      terra.1[positions[c.counter],'ancestor.within.t.ID']<-dc.old[which(dc[]==c.counter)[1]]
    }
  }
  
  # closing link to pdf
  dev.off()
  
  ## CONSTRUCTING TERRA.2
  
  # final year of data (for calculating 'exists' and 'destroyed')
  final.t <- all.yrs[1]
  if (verbose) print(paste("final dataset year:", final.t))
  #now convert the information in terra.1 into the form Rob likes
  terra.2<-data.frame(start.ID=paste(terra.1$t,':',terra.1$within.t.ID,sep=''),
                      end.ID=paste(terra.1$t,':',terra.1$within.t.ID,sep=''),
                      start.t=terra.1$t,
                      end.t=terra.1$t,
                      #start.size=terra.1$size,
                      #end.size=terra.1$size,
                      # removed these for speed
                      start.size=NA,
                      end.size=NA,
                      ancestor.end.ID=paste(terra.1$ancestor.t,':',terra.1$ancestor.within.t.ID,sep=''),
                      end.type=factor(NA,levels=c('exists','destroyed','split')),
                      stringsAsFactors=F)
  terra.2$ancestor.end.ID[is.na(terra.1$ancestor.t)]<-''
  terra.2[terra.1$t==final.t,'end.type']<-'exists'
  terra.2[terra.1$t!=final.t,'end.type']<-'destroyed'
  for (counter in 1:dim(terra.2)[1])
  {
    #find the decendents of the focal chunk, if any
    dinds<-which(terra.2$ancestor.end.ID==terra.2$end.ID[counter])
    
    if (length(dinds)==0) { next }
    
    if (length(dinds)==1)
    {
      terra.2[dinds,c('start.ID','start.t','start.size','ancestor.end.ID')]<-
              terra.2[counter,c('start.ID','start.t','start.size','ancestor.end.ID')]
      terra.2[counter,'start.ID']<-''
    }
    
    if (length(dinds)>1)
    {
      terra.2[counter,'end.type']<-'split'
    }
  }
  terra.2<-terra.2[terra.2$start.ID!='',]

  save(file=paste(outPath, '/terrageny.RData', sep=""), terra.1, terra.2)
}