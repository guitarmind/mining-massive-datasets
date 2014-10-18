
week3_q1 <- function() {
  
  # create graph object
  raw <- rbind(c(0,0,1,0,0,1,0,0),
             c(0,0,0,0,1,0,0,1),
             c(1,0,0,1,0,1,0,0),
             c(0,0,1,0,1,0,1,0),
             c(0,1,0,1,0,0,0,1),
             c(1,0,1,0,0,0,1,0),
             c(0,0,0,1,0,1,0,1),
             c(0,1,0,0,1,0,1,0))
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
  
  sum_L <- sum(L)
  print(paste('Sum of L = ', sum_L))
  
  sum_D <- sum(D)
  print(paste('Sum of D = ', sum_D))
  
  # The Number of Non-Zero Values of a Matrix
  nnzero_L <- nnzero(L)
  print(paste('nnzero of L = ', nnzero_L))
  
  nnzero_D <- nnzero(D)
  print(paste('nnzero of D = ', nnzero_D))
}

# main()
library("igraph")
library("Matrix")
week3_q1()
