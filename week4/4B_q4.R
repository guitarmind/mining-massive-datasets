
week4_4B_q4 <- function() {
  
  x <- rbind(1,2,3)
  #   print(x)
   
  # http://math.stackexchange.com/questions/496662/how-to-find-a-perpendicular-vector
  y <- rbind(1,-2,1)
  print(sum(x*y))
  
  y <- rbind(2,-3,1)
  print(sum(x*y))
  
  y <- rbind(-1,2,-3)
  print(sum(x*y))
  
  y <- rbind(1,0,0)
  print(sum(x*y))
}

# main()
library("lsa")
week4_4B_q4()
