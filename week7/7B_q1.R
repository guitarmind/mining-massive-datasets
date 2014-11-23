
week7_7B_q1 <- function() {
  
  # j -> i
  adj_matrix <- cbind(c(0,1,1,0),
                      c(1,0,0,0),
                      c(0,0,0,1),
                      c(0,0,1,0))
  print(adj_matrix)
  
  beta <- 0.7
  # Assume that pages selected for the teleport set are nodes 1 and 2 and that in the teleport set, 
  # the weight assigned for node 1 is twice that of node 2
  teleport_set <- rbind(1,2)
  teleport_set_len <- 3
  es <- rbind(2, 1, 0, 0)
  # print(es)
  
  # outdegree of nodes
  d <- cbind(sum(adj_matrix[,1]),
             sum(adj_matrix[,2]),
             sum(adj_matrix[,3]),
             sum(adj_matrix[,4]))
  temp <- rbind(1/d,1/d,1/d,1/d)
  #   print(temp)
  M <- adj_matrix * temp
  print(M)
  
  # page rank matrix
  n <- nrow(adj_matrix)
  r <- rbind(1/n,1/n,1/n,1/n)
  # faster way
  # n <- 2
  # r <- rbind(1/n,1/n,0,0)
  print(r)
  
  # 1000 iterations
  oldr <- r
  i <- 1
  repeat{
    print(paste("Iteraton ",i))
    newr <- compute_topical_pagerank(M, oldr, beta, es, teleport_set_len)
    print(newr)
    
    oldr <- newr
    i <- i + 1
    
    if(i > 1000) {
      break
    }
  }
  
}

compute_topical_pagerank <- function(M, r, beta, es, teleport_set_len) {
  newr <- beta * (M %*% r) + (1 - beta) * (es / teleport_set_len)
  return(newr)
}

# main()
week7_7B_q1()
