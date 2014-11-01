
week5_5B_q2 <- function() {
  
  a_b <- c(5,10)
  c_d <- c(20,5)
  
  # (1) Yellow: UL=(6,7) and LR=(11,4); Blue: UL=(14,10) and LR=(23,6) 
  YUL <- c(6,7)
  YLR <- c(11,4)
  BUL <- c(14,10)
  BLR <- c(23,6)
  
  # (2) Yellow: UL=(6,7) and LR=(11,4); Blue: UL=(11,5) and LR=(17,2) 
  # YUL <- c(6,7)
  # YLR <- c(11,4)
  # BUL <- c(11,5)
  # BLR <- c(17,2)
  
  # (3) Yellow: UL=(7,12) and LR=(12,8); Blue: UL=(16,19) and LR=(25,12) 
  # YUL <- c(7,12)
  # YLR <- c(12,8)
  # BUL <- c(16,19)
  # BLR <- c(25,12)
  
  # (4) Yellow: UL=(6,15) and LR=(13,7); Blue: UL=(16,16) and LR=(18,5) 
  # YUL <- c(6,15)
  # YLR <- c(13,7)
  # BUL <- c(16,16)
  # BLR <- c(18,5)
  
  # (5) Yellow: UL=(3,3) and LR=(10,1); Blue: UL=(15,14) and LR=(20,10) 
  # YUL <- c(3,3)
  # YLR <- c(10,1)
  # BUL <- c(15,14)
  # BLR <- c(20,10)
  
  # (6) Yellow: UL=(6,15) and LR=(13,7); Blue: UL=(16,19) and LR=(25,12) 
  # YUL <- c(6,1)
  # YLR <- c(13,7)
  # BUL <- c(16,19)
  # BLR <- c(25,12)
  
  # (7) Yellow: UL=(7,8) and LR=(12,5); Blue: UL=(13,10) and LR=(16,4) 
  # YUL <- c(7,8)
  # YLR <- c(12,5)
  # BUL <- c(13,10)
  # BLR <- c(16,4)
  
  # (8) Yellow: UL=(7,8) and LR=(12,5); Blue: UL=(15,14) and LR=(20,10) 
  # YUL <- c(7,8)
  # YLR <- c(12,5)
  # BUL <- c(15,14)
  # BLR <- c(20,10)
  
  # (9) Yellow: UL=(3,15) and LR=(13,7); Blue: UL=(14,10) and LR=(23,6) 
  # YUL <- c(3,15)
  # YLR <- c(13,7)
  # BUL <- c(14,10)
  # BLR <- c(23,6)
  
  # (10) Yellow: UL=(7,12) and LR=(12,8); Blue: UL=(16,16) and LR=(18,5) 
  # YUL <- c(7,12)
  # YLR <- c(12,8)
  # BUL <- c(16,16)
  # BLR <- c(18,5)
  
  # (11) Yellow: UL=(7,8) and LR=(12,5); Blue: UL=(13,10) and LR=(16,4) 
  # YUL <- c(7,8)
  # YLR <- c(12,5)
  # BUL <- c(13,10)
  # BLR <- c(16,4)
  
  
  print("Yellow")
  compute_closest_centroid(YUL, YLR, a_b, c_d)
  #   compute_min_corner_dist(YUL, YLR, a_b)
  #   compute_min_corner_dist(YUL, YLR, c_d)
  
  print("Blue")
  compute_closest_centroid(BUL, BLR, a_b, c_d)
  #   compute_min_corner_dist(BUL, BLR, a_b)
  #   compute_min_corner_dist(BUL, BLR, c_d)
  
  # show chart
  plot(rbind(a_b[1],c_d[1]),
       rbind(a_b[2],c_d[2]),
       type="p",
       xlab="X-axis",ylab="y-axix",asp=1,
       xlim=c(0, 30),ylim=c(0, 30),
       pch="*",col="black",bg="grey")
  points(rbind(YUL[1],YLR[1]), 
         rbind(YUL[2],YLR[2]),
         type="p",pch=16,col="orange")
  points(rbind(BUL[1],BLR[1]), 
         rbind(BUL[2],BLR[2]),
         type="p",pch=16,col="blue")
  # draw point label
  allpoints <- rbind(a_b,c_d,YUL,YLR,BUL,BLR)
  textxy(allpoints[,1],allpoints[,2],offset=0.7,cex=0.6,
         labs=c(paste(paste("(",allpoints[,1],","),allpoints[,2],")")))
  
  # graphics.off()
  
  
}

compute_closest_centroid <- function(UL, LR, centroid_1, centroid_2) {
  UR <- c(LR[1], UL[2])
  LU <- c(UL[1], LR[2])
  #   print(UR)
  #   print(LU)
  corners <- rbind(UL,LR,UR,LU)
  for(i in seq(from=1, to=nrow(corners), by=1)) {
    compare_cetroid_dist(corners[i,], centroid_1, centroid_2)
  }
}

compare_cetroid_dist <- function(p,centroid_1, centroid_2) {
  c1_dist <- euclidean_dist(p,centroid_1)
  c2_dist <- euclidean_dist(p,centroid_2)
  
  closest_centroid <- 0
  min_dist <- -999
  if(c1_dist < c2_dist) {
    min_dist <- c1_dist
    print(paste("centroid 1 is closer: ",min_dist))
    closest_centroid <- 1
  }
  else {
    if(c1_dist > c2_dist) {
      min_dist <- c2_dist
      print(paste("centroid 2 is closer",min_dist))
      closest_centroid <- 2
    }
  }
  
  return(closest_centroid)
}

compute_min_corner_dist <- function(UL, LR, centroid) {
  UR <- c(LR[1], UL[2])
  LU <- c(UL[1], LR[2])
  #   print(UR)
  #   print(LU)
  
  min_dist <- 999
  corners <- rbind(UL,LR,UR,LU)
  for(i in seq(from=1, to=nrow(corners), by=1)) {
    dist <- euclidean_dist(corners[i,],centroid)
    
    if(i == 1) {
      min_dist <- dist
    }
    else {
      if(min_dist > dist) {
        min_dist <- dist
      }
    }
  }
  
  print(paste(paste("min_dist to ", 
                    paste(centroid[1], ",", centroid[2]), ":"), min_dist))
  return(min_dist)
}

euclidean_dist <- function(p1, p2) {
  dist = sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(dist)
}

# main()
library("calibrate")
week5_5B_q2()
