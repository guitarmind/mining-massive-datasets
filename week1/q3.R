
week1_q3 <- function() {
  
  G <- rbind(c(0,1,1),c(0,0,1),c(1,0,0))
  print(G)
  g <- graph.adjacency(G, mode="directed")
  result <- page.rank(g, algo="prpack", damping=0.999)$vector
  
  result <- 3 * result
  
  # a
  print(paste('a = ', result[1]))
  # b
  print(paste('b = ', result[2]))
  # c
  print(paste('c = ', result[3]))
  # a + b + c
  print(paste('a + b + c = ', result[1] + result[2] + result[3]))
}

# main()
library("igraph")
week1_q3()
