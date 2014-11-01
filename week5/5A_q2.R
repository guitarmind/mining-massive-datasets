
week5_5A_q2 <- function() {
  table <- rbind(c(0.1,0.015,0.010,0.005,1,0.0,0.0,0.0),
                 c(0.09,0.016,0.012,0.006,2,0.0,0.0,0.0),
                 c(0.08,0.017,0.014,0.007,3,0.0,0.0,0.0),
                 c(0.07,0.018,0.015,0.008,4,0.0,0.0,0.0),
                 c(0.06,0.019,0.016,0.010,5,0.0,0.0,0.0))
  colnames(table) <- c("Bid","CTR1","CTR2","CTR3","Budget","EV1","EV2","EV3")
  rownames(table) <- c("A","D","C","D","E")
  
  # compute expected revenue
  table[,6] <- table[,1] * table[,2]
  table[,7] <- table[,1] * table[,3]
  table[,8] <- table[,1] * table[,4]
  
  print(table)
  
  maxer_idx1 <- which.max(table[,6])
  maxer_idx2 <- which.max(table[,7])
  maxer_idx3 <- which.max(table[,8])
  
  #   print(maxer_idx1)
  #   print(maxer_idx2)
  #   print(maxer_idx3)
  
  click_through_to_auction <- 101
  
  table[maxer_idx1[1],5] <- table[maxer_idx1[1],5] - table[maxer_idx1[1],1]
  table[maxer_idx2[1],5] <- table[maxer_idx2[1],5] - table[maxer_idx2[1],1]
  table[maxer_idx3[1],5] <- table[maxer_idx3[1],5] - table[maxer_idx3[1],1]
  print(table)
  
  
  # writeLines("\n\n")
}

# main()
week5_5A_q2()
