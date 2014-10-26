
week4_4B_q1 <- function() {
  
  x <- 0
  y <- 0
  z <- 0
  M <- rbind(c(2/7,x),
             c(3/7,y),
             c(6/7,z))
  
  #   print(M)
  
  # [.312, .156, -.937] 
  x <- .312
  y <- .156
  z <- -.937
  M <- rbind(c(2/7,x),
             c(3/7,y),
             c(6/7,z))
  print(M)
  print("M[,1] * M[,2] = ")
  #   print(t(M) %*% M)
  print(rbind(M[,1] %*% M[,2]))
  
  # [.429, .857, .286] 
  x <- .429
  y <- .857
  z <- .286
  M <- rbind(c(2/7,x),
             c(3/7,y),
             c(6/7,z))
  print(M)
  print("M[,1] * M[,2] = ")
  #   print(t(M) %*% M)
  print(rbind(M[,1] %*% M[,2]))
  
  # [-.937, .312, .156]  
  x <- -.937
  y <- .312
  z <- .156
  M <- rbind(c(2/7,x),
             c(3/7,y),
             c(6/7,z))
  print(M)
  print("M[,1] * M[,2] = ")
  #   print(t(M) %*% M)
  print(rbind(M[,1] %*% M[,2]))
  
  # [.954, .728, -.682]
  x <- .954
  y <- .728
  z <- -.682
  M <- rbind(c(2/7,x),
             c(3/7,y),
             c(6/7,z))
  print(M)
  print("M[,1] * M[,2] = ")
  #   print(t(M) %*% M)
  print(rbind(M[,1] %*% M[,2]))
}

# main()
library("polynom") 
week4_4B_q1()
