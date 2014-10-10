
week1_q2 <- function() {
  
  G <- rbind(c(0,1,1),c(0,0,1),c(1,0,0))
  print(G)
  g <- graph.adjacency(G, mode="directed")
  result <- page.rank(g, algo="prpack", damping=0.85)$vector
  
  result <- 3 * result
  
  # a
  print(paste('a = ', result[1]))
  # b
  print(paste('b = ', result[2]))
  # c
  print(paste('c = ', result[3]))
  # a + b + c
  print(paste('a + b + c = ', result[1] + result[2] + result[3]))
  
  # c = b + .575a
  print(paste('b + .575a => ', result[2] + 0.575 * result[1]))
  
  # 85b = .575a + .15c 
  print(paste('85b => ', 85 * result[2]))
  print(paste('.575a + .15c => ', 0.575* result[1] + 0.15 * result[3]))
  
  # .95a = .9c + .05b 
  print(paste('.95a => ', 0.95 * result[1]))
  print(paste('.9c + .05b => ', 0.9 * result[3] + 0.05 * result[2]))
  
  # b = .475a + .05c
  print(paste('.475a + .05c => ', 0.475 * result[1] + 0.05 * result[3]))
  
  # .85c = b + .575a 
  print(paste('.85c  => ', 0.85 * result[3]))
  print(paste('b + .575a => ', result[2] + 0.575 * result[1]))
  
  # .85a = c + .15b 
  print(paste('.85a  => ', 0.85 * result[1]))
  print(paste('c + .15b => ', result[3] + 0.15 * result[2]))
  
  # .95c = .9b + .475a 
  print(paste('.95c  => ', 0.95 * result[3]))
  print(paste('.9b + .475a  => ', 0.9 * result[2] + 0.475 * result[1]))
  
}

# main()
library("igraph")
week1_q2()
