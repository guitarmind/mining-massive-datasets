
week7_7A_q3 <- function() {
  
  # i -> j
  L <- rbind(c(0,1,1,0),
             c(1,0,0,0),
             c(0,0,0,1),
             c(0,0,1,0))
  
  print(L)
  
  # j -> i
  L_transpose <- t(L)
  print(L_transpose)
  
  # Start by assuming the hubbiness of each node is 1; 
  # that is, the vector h is (the transpose of) [1,1,1,1]
  h <- cbind(rep(1,4))
  # print(h)
  
  # 1000 iterations
  oldh <- h
  olda <- NULL
  i <- 1
  repeat{
    print(paste("Iteraton ",i))
    output <- compute_hits(oldh, L, L_transpose)
    
    newh <- cbind(output[,1])
    newa <- cbind(output[,2])
    
    writeLines("\nnew a:")
    print(newa)
    writeLines("new h:")
    print(newh)
    
    oldh <- newh
    olda <- newa
    i <- i + 1
    
    if(i > 2) {
      break
    }
  }
  
}

compute_hits <- function(h, L, L_transpose) {
  # print(dim(L_transpose))
  # print(dim(h))
  
  # Compute an estimate of the authority vector a=LTh
  newa <- L_transpose %*% h
  # print(newa)
  
  # Normalize a by dividing all values so the largest value is 1
  newa <- newa / max(newa)
  # print(newa)
  
  # Compute an estimate of the hubbiness vector h=La
  newh <- L %*% newa
  # print(newh)
  
  # Normalize h by dividing all values so the largest value is 1
  newh <- newh / max(newh)
  # print(newh)
  
  return(cbind(newh, newa))
}

convergence_check <- function(h, newh, a, newa, epsilon) {
  
}

# main()
week7_7A_q3()
