
week3_q2 <- function() {
  
  # create graph object
  raw <- rbind(c(0,1,1,0,0,0),
               c(1,0,0,1,0,1),
               c(1,0,0,1,0,0),
               c(0,1,1,0,1,0),
               c(0,0,0,1,0,1),
               c(0,1,0,0,1,0))
  print(raw)
  g <- graph.adjacency(raw, mode="undirected")
  
  # print(dim(raw))
  n <- nrow(raw)
  # print(n)
  
  # convert a graph to an adjacency matrix or an edge list
  A <- get.adjacency(g)
  print(A)
  D <- matrix(0,n,n)
  for(i in seq(from=1, to=n, by=1)) {
    diag(D)[i] <- sum(A[i,])
  }
  print(D)
  L <- D-A 
  
  print(L)
  # another way
  print(graph.laplacian(g))
  
  # Community structure detecting based on the leading eigenvector of the community matrix
  lec <- leading.eigenvector.community(g)
  print(lec)
  
  # compute eigenvalues and eigenvectors of L
  print(eigen(L, symmetric=TRUE, only.values=FALSE))
  eigenVec = eigen(L, symmetric=TRUE, only.values=FALSE)$vectors
  
  # compute mean of 2nd smallest eigen vector
  mean <- mean(eigenVec[,5])
  print(paste('Mean of 2nd smallest eigen vector = ', mean))
  
  # cluster 1
  print(cbind(eigenVec[eigenVec[,5] > mean, 5]))
  
  # cluster 2
  print(cbind(eigenVec[eigenVec[,5] < mean, 5]))

}

# main()
library("igraph")
library("Matrix")
week3_q2()
