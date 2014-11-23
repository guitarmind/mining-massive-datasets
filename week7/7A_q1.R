
week7_7A_q1 <- function() {

  # LSH family h of (d1,d2,.6,.4)
  d1 <- NULL
  d2 <- NULL
  p1 <- 0.6
  p2 <- 0.4
  
  # AND-construction
  # r = 3
  r <- 3
  writeLines("\n\nAND-construction:")
  print(paste("w=", p1^3))
  print(paste("x=", p2^3))
  
  # OR-construction
  # b = 2
  b <- 2
  writeLines("\n\nOR-construction:")
  print(paste("y=", 1-(1-p1)^b))
  print(paste("z=", 1-(1-p2)^b))
  
}

# main()
library("polynom") 
week7_7A_q1()
