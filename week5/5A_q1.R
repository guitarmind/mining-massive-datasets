
week5_5A_q1 <- function() {
  M <- rbind(c(1,0,0),
             c(0,2,0),
             c(0,0,0))
  print(M)
  writeLines("\n\nPseudoinverse of M:")
  print(pseudoinverse(M))
}

# main()
library("corpcor")
week5_5A_q1()
