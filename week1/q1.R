
week1_q1 <- function() {
  #  G <- read.table("q1_dataset.txt", header = FALSE)
  G <- rbind(c(0,1,1),c(0,0,1),c(0,0,1))
  print(G)
  g <- graph.adjacency(G, mode="directed")
  # g <- random.graph.game(20, 5/20, directed=TRUE)
  result <- page.rank(g, algo="prpack", damping=0.7)$vector
  
  #   G <- rbind(c(0,1,1),c(0,0,1),c(0,0,1))
  #   print(G)
  #   result <- pagerank_markpeng(G,'power', 0.7)
  # result <- pagerank(G,'eigen', 0.7)
  #   print(result)
  
  result <- 3 * result
  
  # ra + rb
  print(paste('a + b = ', result[1] + result[2]))
  # rb + rc
  print(paste('b + c = ', result[2] + result[3]))
  # ra + rc
  print(paste('a + c = ', result[1] + result[3]))
  # ra + rb + rc
  print(paste('a + b + c = ', result[1] + result[2] + result[3]))
  
}

pagerank_markpeng <- function(G,method='eigen',d=.85,niter=100){
  # follows the notation of the matlab article on pagerank.
  # at http://www.mathworks.com/company/newsletters/news_notes/clevescorner/oct02_cleve.html
  # G is a connectivity matrix, with G[i,j]=1 if page i points to page j
  # method is either "power" or "eigen"
  
  # compute indegree of each node
  cvec <- apply(G,2,sum)
  # print(cvec)
  cvec[cvec==0] <- 1 # nodes with indegree 0 will cause problems if we divide by 0.
  # print(cvec)
  
  # compuate outdegree of each node
  gvec <- apply(G,1,sum)
  # print(gvec)
  
  n <- nrow(G)
  #   print(n)
  delta <- (1-d)/n
  #   print(delta)
  
  A <- matrix(0,nrow(G),ncol(G))
  
  oldR <- cbind(1, 1, 1)
  for (i in 1:n) 
  {
    for (j in 1:n)
    {
      
    }
  }
}

pagerank <- function(G,method='eigen',d=.85,niter=100){
  # follows the notation of the matlab article on pagerank.
  # at http://www.mathworks.com/company/newsletters/news_notes/clevescorner/oct02_cleve.html
  # G is a connectivity matrix, with G[i,j]=1 if page i points to page j
  # method is either "power" or "eigen"
  
  # compute indegree of each node
  cvec <- apply(G,2,sum)
  # print(cvec)
  cvec[cvec==0] <- 1 # nodes with indegree 0 will cause problems if we divide by 0.
  # print(cvec)
  
  # compuate outdegree of each node
  gvec <- apply(G,1,sum)
  # print(gvec)
  
  n <- nrow(G)
  print(n)
  delta <- (1-d)/n
  print(delta)
  
  A <- matrix(delta,nrow(G),ncol(G))
  
  print(A)
  for (i in 1:n) 
    A[i,] <- A[i,] + d*G[i,]/cvec
  #  print(A)
  
  if (method=='power'){
    x <- rep(1,n)
    for (i in 1:niter) x <- A%*%x
  } else {
    x <- Re(eigen(A)$vector[,1])
  }
  x/sum(x)
}

# main()
library("igraph")
week1_q1()

