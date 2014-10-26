
week4_4B_q3 <- function() {
  
  M <- rbind(c(1,1),
             c(2,2),
             c(3,4))
  print(M)
  print(t(M))
  
  MTM <- t(M) %*% M
  print(MTM)
  
  # M * e = lambda * e
  # (M - lambda * I) * e = 0
  
  # Find zeros of a real or complex polynomial
  lambda <- 0
  # eq <- (14 - lambda) * (21 - lambda) - (17 * 17) 
  eq <- polynomial(c(5,-35,1))
  print(eq)
  result <- solve(eq) 
  print(result)
  
  # validate
  print((0.143445*0.143445) - (35 * 0.143445))
  print((34.856555*34.856555) - (35 * 34.856555))
  
  # compute eigenvectors of MTM directly
  print(eigen(MTM))
  E <- eigen(MTM)$vectors
  print(E)
  ME <- M %*% E
  print("ME:")
  print(ME)
}

# main()
library("polynom") 
week4_4B_q3()
