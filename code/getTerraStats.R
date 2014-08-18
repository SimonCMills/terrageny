
load("scenes/scene_665/clumped/terrageny.RData")
View (terra.2)

# interested in fragments that exist in 2013
terra <- terra.2[which(terra.2$end.type == "exists"),]
# ignoring fragments which have remained unchanged since time 0 (and are therefore
# completely unconnected)
terra <- terra[-which(terra$ancestor.end.ID == ""),]
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


for (row_i in 1:nrow(nodesMatrix)) {  
  # getting fragment history for i
  frag_i <- nodesMatrix[row_i,]
  # creating matrix which can edit within this iterate
  nodesMatrix_edit <- nodesMatrix
  # setting to NA so comparisons aren't made with self
  nodesMatrix_edit[row_i] <- NA
  # going over this 
  node_ID <- which(!is.na(frag_i))
  # going through nodes for this fragment, 
  for (node_i in node_ID[-1]) {
    # getting row-ID of pairs with this fragment node
    node_ID <- as.numeric(which(nodesMatrix_edit[,node_i] == frag_i[node_i]))
    node
    pairedNodes <- nodesMatrix_edit[node_ID,1:node_i]
    pairedNodes
    # creating vector that assumes every year split
    nodesDefault <- rep(ncol(pairedNodes), nrow(pairedNodes))
    nodesDefault
    # creating 0-valued vector of same length
    NAcount <- rep(0, nrow(pairedNodes))
    # looping through years, counting years in which a split wasn't made (NA)
    for (i in 1:ncol(pairedNodes)-1) {
      colNAs <- which(is.na(pairedNodes[,i]))
      NAcount[colNAs] <- NAcount[colNAs] + 1
    }
    # updating pairwise matrix
    pairwiseMatrix_nodes[as.numeric(rownames(pairedNodes)), row_i] <- nodesDefault - NAcount
    nodesMatrix_edit[rownames(pairedNodes),] <- NA
  }  
}
    

# loop over years 
 which(nodesMatrix[,4] == unique(nodesMatrix[,4])[1])