
week4_q2 <- function() {
  
  # alpha = 0
  a <- 0
  X <- rbind(c(1,0,1,0,1,2*a),
             c(1,1,0,0,1,6*a),
             c(0,1,0,1,0,2*a))
  print(X)
  
  n <- nrow(X) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  #   print(cmb)
  C <- matrix(apply(cmb,1,cos.sim, X),n,n)
  rownames(C) <- c("A", "B", "C")
  colnames(C) <- c("A", "B", "C")
  print("alpha = 0")
  print(C)
  
  # alpha = 0.5
  a <- 0.5
  X <- rbind(c(1,0,1,0,1,2*a),
             c(1,1,0,0,1,6*a),
             c(0,1,0,1,0,2*a))
  print(X)
  
  n <- nrow(X) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  #   print(cmb)
  C <- matrix(apply(cmb,1,cos.sim, X),n,n)
  rownames(C) <- c("A", "B", "C")
  colnames(C) <- c("A", "B", "C")
  print("alpha = 0.5")
  print(C)
  
  # alpha = 1
  a <- 1
  X <- rbind(c(1,0,1,0,1,2*a),
             c(1,1,0,0,1,6*a),
             c(0,1,0,1,0,2*a))
  print(X)
  
  n <- nrow(X) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  #   print(cmb)
  C <- matrix(apply(cmb,1,cos.sim, X),n,n)
  rownames(C) <- c("A", "B", "C")
  colnames(C) <- c("A", "B", "C")
  print("alpha = 1")
  print(C)  
  
  # alpha = 2
  a <- 2
  X <- rbind(c(1,0,1,0,1,2*a),
             c(1,1,0,0,1,6*a),
             c(0,1,0,1,0,2*a))
  print(X)
  
  n <- nrow(X) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  #   print(cmb)
  C <- matrix(apply(cmb,1,cos.sim, X),n,n)
  rownames(C) <- c("A", "B", "C")
  colnames(C) <- c("A", "B", "C")
  print("alpha = 2")
  print(C)
  
  
}

# Angular similarity
# http://en.wikipedia.org/wiki/Cosine_similarity#Angular_similarity
# cos.sim <- function(ix, X) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return (1 - ((1 /(sum(A*B)/sqrt(sum(A^2)*sum(B^2)))) / pi))
# }

# cos.sim <- function(ix, X) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return (1 - ((2*(1 /(sum(A*B)/sqrt(sum(A^2)*sum(B^2))))) / pi))
# }

# Cosine Disance
cos.sim <- function(ix, X) 
{
  A = X[ix[1],]
  B = X[ix[2],]
  return (1 - (sum(A*B)/sqrt(sum(A^2)*sum(B^2))))
}

# cos.sim <- function(ix, X) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
# }

# main()
library("lsa")
week4_q2()
