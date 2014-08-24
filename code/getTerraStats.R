#library(raster)
rm(list=ls())
load("terrageny.RData")
#load("../terragenyLocal/terragenyConstruct/manaus/Results.RData")
#
#source("code/functions/createTerrUT.R")
#source("code/functions/constructTerrageny_ver2.R")
#createTerrUT(unitTestDir="ut_terrstats/", yearRange=2000:2012)
#constructTerrageny(inPath="ut_terrstats/dynamicSingle/", outPath="ut_terrstats/", inFileStart="year_", 
#                   fileType=".tif", overwrite=T)
load("ut_terrstats/terrageny.RData")
#View (terra.2)

# interested in fragments that exist in 2013
terra <- terra.2[which(terra.2$end.type == "exists"),]
# ignoring fragments which have remained unchanged since time 0 (and are therefore
# completely unconnected)
if(any(terra$ancestor.end.ID == "")) {
  terra <- terra[-which(terra$ancestor.end.ID == ""),]
}
nExists <- length(terra$start.ID)
# constructing pairwise nodes matrix
pairwiseMatrix_nodes <- matrix(NA, nrow=nExists, ncol=nExists)
# creating matrix containing fragment histories (recording each split)
nodesMatrix <- matrix(NA, nrow=length(terra$start.ID), ncol=13, 
                      dimnames=list(1:length(terra$start.ID), 2012:2000))
# looping through fragments to construct history
for (i in 1:length(terra$start.ID)) {
  # assigning fragment start node
  nodesMatrix[i, as.character(terra$start.t)[i]] <- terra$start.ID[i]
  # assigning fragment ancestor node
  ancestor.list <- as.character(terra$ancestor.end.ID)
  # getting year-component of this 
  ancestor.t <- substr(ancestor.list, 1, 4)
  # placing ID in matrix
  nodesMatrix[i, ancestor.t[i]] <- terra$ancestor.end.ID[i]
  # assigning ancestor's ancestor node
  frag_i <- terra[i,]
  frag_i_ancestor <- frag_i$ancestor.end.ID
  frag_i_ancestor.t <- substr(frag_i_ancestor, 1, 4)
  nodesMatrix[i, frag_i_ancestor.t] <- frag_i_ancestor
  #looping through
  while(frag_i_ancestor.t != "2000") {
    frag_i1 <- terra.2[which(terra.2$end.ID == frag_i_ancestor),]
    frag_i_ancestor <- frag_i1$ancestor.end.ID
    if(frag_i_ancestor == "") break()
    frag_i_ancestor.t <- substr(frag_i1$ancestor.end.ID, 1, 4)
    nodesMatrix[i, frag_i_ancestor.t] <- frag_i_ancestor
  }
  nodesMatrix[i, frag_i_ancestor.t] <- frag_i_ancestor
}

View(nodesMatrix)
################################################################################
pairwiseNodal <- matrix(NA, nrow=nExists, ncol=nExists)
pairwiseDist <- matrix(NA, nrow=nExists, ncol=nExists)

# loop over years in nodesMatrix (2011:2000)
for (year_i in as.character(2000:2012)) {
  # get list of nodal events in year_i (ignoring NA)
  nodes <- unique(nodesMatrix[,year_i])
  if (any(is.na(nodes))) {nodes <- nodes[-which(is.na(nodes))]}
  # counter
  print("year:")
  print(year_i)
  # loop through nodal events, getting number of nodes separating pairs
  for (node_i in 1:length(nodes)) {
    print(paste("node", node_i))
    nodalFragments <- which(nodesMatrix[,year_i] == nodes[node_i])
    # if just one extant fragment associated with this node (i.e. itself), skip
    if (length(nodalFragments) != 1) {
      # creating blank vector of same length
      nodalFragments_splits <- rep(NA, length(nodalFragments))
      # looping through years, counting years in which a split was made (!=NA)
      for (fragment_i in 1:length(nodalFragments)) {
        yearVect <- 2012:as.numeric(year_i)
        splits <- which(!is.na(nodesMatrix[nodalFragments[fragment_i], as.character(yearVect)]))
        nSplits <- length(splits)
        if (nSplits == 0) {stop("nSplits is 0")}
        nodalFragments_splits[fragment_i] <- nSplits
      }
      #print(nodalFragments_splits)
      pairwiseNodes_node_i <- matrix(NA, nrow=length(nodalFragments_splits), 
                                ncol=length(nodalFragments_splits), 
                                dimnames=list(nodalFragments, nodalFragments))
      
      for (pair_i in 1:length(nodalFragments_splits)) {
        # -1 to account for shared node at split
        pairwiseNodes_node_i[,pair_i] <- nodalFragments_splits + nodalFragments_splits[pair_i] - 1
      }
      age <- (2012-as.numeric(year_i))*2 - 2
      for (i in colnames(pairwiseNodes_node_i)) {
        for (j in rownames(pairwiseNodes_node_i)) {
          pairwiseNodal[as.numeric(j), as.numeric(i)] <- pairwiseNodes_node_i[j,i]
          pairwiseDist[as.numeric(j), as.numeric(i)] <- age
        }
      }
    }
  }
  # removes cells above diagonal, which are mirror image 
  pairwiseNodal[upper.tri(pairwiseNodal, diag=T)] <- NA
  pairwiseDist[upper.tri(pairwiseDist, diag=T)] <- NA
}
min(pairwiseDist, na.rm=T)
hist(pairwiseNodal)
xx <- pairwiseNodal 
xx[upper.tri(xx)] <- NA
image(pairwiseDist)
################################################################################
# function 2: computing terragenetic distinctiveness. 
timestep <- as.character(2000:2012)

# variables
time.start <- as.character(2000)
time.finish <- as.character(2012)

# need to compute nodesMatrix from terra.2
# interested in fragments that exist in 2013
terra.exists <- which(terra.2$end.type == "exists")
terra.extinct <- which(terra.2$end.type == "destroyed")
# ignoring fragments which have remained unchanged since time 0 (and are therefore
# completely unconnected)
terra.exists <- terra.exists[-which(terra.2$ancestor.end.ID[terra.exists] == "")]
n.exists <- length(terra.exists)
# creating matrix containing fragment histories (recording each split)
nodes.matrix <- matrix(NA, nrow=length(terra.exists), ncol=13, 
                       dimnames=list(1:length(terra.exists), 2012:2000))


# assigning fragment start node (as vector)
start.yr.list <- terra.2$start.t[terra.exists]
start.id.list <- terra.2$start.ID[terra.exists]
# assigning fragment ancestor node
ancestor.list <- as.character(terra.2$ancestor.end.ID[terra.exists])
# getting year-component of this 
ancestor.t <- substr(ancestor.list, 1, 4)

# looping through fragments to construct history
# note: this matrix does not retain original row ids, these are kept in terra.exists
for (i in 1:n.exists) {
  nodes.matrix[i, as.character(start.yr.list)[i]] <- start.id.list[i]
  # placing ID in matrix
  nodes.matrix[i, ancestor.t[i]] <- terra.2$ancestor.end.ID[terra.exists[i]]
  # assigning ancestor's ancestor node
  frag.i <- terra.2[terra.exists[i],]
  frag.i.ancestor <- frag_i$ancestor.end.ID
  frag_i_ancestor.t <- substr(frag_i_ancestor, 1, 4)
  nodesMatrix[i, frag_i_ancestor.t] <- frag_i_ancestor
  #looping through
  while(frag_i_ancestor.t != time.start) {
    frag_i1 <- terra.2[which(terra.2$end.ID == frag_i_ancestor),]
    frag_i_ancestor <- frag_i1$ancestor.end.ID
    if(frag_i_ancestor == "") break()
    frag_i_ancestor.t <- substr(frag_i1$ancestor.end.ID, 1, 4)
    nodes.matrix[i, frag_i_ancestor.t] <- frag_i_ancestor
  }
  nodes.matrix[i, frag_i_ancestor.t] <- frag_i_ancestor
}





# loop over years in nodesMatrix (2011:2000)
for (year_i in 1:length(timestep)) {
  year_i <- "2001"
  # get list of nodal events in year_i (ignoring NA)
  nodes <- unique(nodesMatrix[,year_i])
  nodes
  if (any(is.na(nodes))) {nodes <- nodes[-which(is.na(nodes))]}
  nodes
  # counter
  print("year:")
  print(timestep[year_i])
  
  # loop through nodal events, getting number of nodes separating pairs
  for (node_i in 1:length(nodes)) {
    node_i <- 1
    print(paste("node", node_i))
    nodalFragments <- which(nodes.matrix[,year_i] == nodes[node_i])
    
    nodalDescendants <- which(terra.2$ancestor.end.ID == nodes[node_i])
    nodalDescendants.1 <- which(terra.2$start.ID == nodes[node_i])
    nodalDescendants.1
    nodalDescendants
    nodes[node_i]
    terra.2[9,]
    terra.2[35,]
    terra.2[220,]
    which(nodes.matrix[,"2004"] == "2004:1")
    
    nodalFragments
    print(nodalFragments)
    
    # getting next timestep in which split happens
    if (any(is.na(nodesMatrix[nodalFragments, timestep[year_i+1]]))) {
      isNA <- TRUE
      NAcount <- 1
      while (any(is.na(nodesMatrix[nodalFragments, timestep[year_i+NAcount]]))) {
        nodesMatrix[nodalFragments, timestep[year_i+NAcount]]
        NAcount <- NAcount+1
      }
    }
    timestep[year_i+NAcount]
    unique(nodesMatrix[nodalFragments, timestep[year_i+NAcount]])
    
    nodesMatrix[nodalFragments, as.character(as.numeric(year_i)-)]
    
    nodesMatrix[nodalFragments,]
    nodesMatrix[nodalFragments, as.character(as.numeric(year_i)-3)]
    # if just one extant fragment associated with this node (i.e. itself), skip
    if (length(nodalFragments) != 1) {
      # creating blank vector of same length
      nodalFragments_splits <- rep(NA, length(nodalFragments))
      # looping through years, counting years in which a split was made (!=NA)
      for (fragment_i in 1:length(nodalFragments)) {
        yearVect <- 2012:as.numeric(year_i)
        splits <- which(!is.na(nodesMatrix[nodalFragments[fragment_i], as.character(yearVect)]))
        nSplits <- length(splits)
        if (nSplits == 0) {stop("nSplits is 0")}
        nodalFragments_splits[fragment_i] <- nSplits
      }
      print(nodalFragments_splits)
      pairwise <- matrix(NA, nrow=length(nodalFragments_splits), ncol=length(nodalFragments_splits), 
                         dimnames=list(nodalFragments, nodalFragments))
      for (pair_i in 1:length(nodalFragments_splits)) {
        # -1 to account for shared node at split
        pairwise[,pair_i] <- nodalFragments_splits + nodalFragments_splits[pair_i] - 1
      }
      for (i in colnames(pairwise)) {
        for (j in rownames(pairwise)) {
          pairwiseNodal[as.numeric(j), as.numeric(i)] <- pairwise[j,i]
        }
      }
    }
  }
  diag(pairwiseNodal) <- NA
}