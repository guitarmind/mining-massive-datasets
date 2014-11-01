
week5_5B_q1 <- function() {
  
  ypoints <- rbind(c(28,145),
                   c(65,140),
                   c(50,130),
                   c(55,118),
                   c(38,115),
                   c(50,90),
                   c(63,88),
                   c(43,83),
                   c(50,60),
                   c(50,30))
  colnames(ypoints) <- c("x", "y")
  
  gpoints <- rbind(c(25,125),
                   c(44,105),
                   c(29,97),
                   c(35,63),
                   c(55,63),
                   c(42,57),
                   c(23,40),
                   c(64,37),
                   c(33,22),
                   c(55,20))
  colnames(gpoints) <- c("x", "y")
  #   rownames(ypoints) <- c("g1","g2","g3","g4","g5",
  #                          "g6","g7","g8","g9","g10")
  
  allpoints <- rbind(ypoints,gpoints)
  
  print("Initial green Centriods:")
  print(gpoints)
  
  # show chart
  plot(ypoints[,1],ypoints[,2],type="p",
       xlab="X-axis",ylab="y-axix",asp=1,
       xlim=c(0, 160),ylim=c(0, 160),
       pch=21,col="dark orange",bg="dark orange")
  points(gpoints[,1], gpoints[,2],type="p",pch=16,col="dark green")
  # draw point label
  textxy(allpoints[,1],allpoints[,2],offset=0.7,cex=0.6,
         labs=c(paste(paste("(",allpoints[,1],","),allpoints[,2],")")))
  
  
  ###################################################################
  # compute Euclidean distance between each green point and centriods
  ###################################################################
  
  nrows <- nrow(ypoints)
  min_vec <- matrix(nrow=nrows,ncol=4)
  colnames(min_vec) <- c("cluster", "cx", "cy", "dist")
  for(i in seq(from=1, to=nrows, by=1)) {
    yellow_i = ypoints[i,]
    
    cluster <- 0
    min_dist <- 0
    for(j in seq(from=1, to=nrows, by=1)) {
      green_j = gpoints[j,]
      
      D_i_j = euclidean_dist(yellow_i, green_j)
      #       print(D_i_j)
      
      if(j == 1) {
        cluster <- 1
        min_dist <- D_i_j
        min_vec[i,2:3] <- green_j
      }
      else {
        if(min_dist > D_i_j) {
          cluster <- j
          min_dist <- D_i_j
          min_vec[i,2:3] <- green_j
        }
      }
    }
    
    min_vec[i,1] <- cluster
    min_vec[i,4] <- min_dist
  }
  
  #   print(min_vec)
  first_result <- cbind(ypoints, min_vec)
  print("first_result:")
  # sort by cluster index
  first_result <- first_result[order(first_result[,3]),]
  print(first_result)
  
  #######################
  # compute new centroids
  #######################
  
  # assign key-value pair(s) to a hash
  sum_1 <- NULL
  sum_2 <- NULL
  sum_3 <- NULL
  sum_5 <- NULL
  sum_10 <- NULL
  for(i in seq(from=1, to=nrow(first_result), by=1)) {
    if(first_result[i,3] == 1) {
      tmp <- rbind(first_result[i,1:2],
                   first_result[i,4:5])
      sum_1 <- rbind(sum_1, tmp)
      sum_1 <- sum_1[!duplicated(sum_1[,1:2]),]
      #       print(sum_1)
    }
    
    if(first_result[i,3] == 2) {
      tmp <- rbind(first_result[i,1:2],
                   first_result[i,4:5])
      sum_2 <- rbind(sum_2, tmp)
      sum_2 <- sum_2[!duplicated(sum_2[,1:2]),]
    }
    
    if(first_result[i,3] == 3) {
      tmp <- rbind(first_result[i,1:2],
                   first_result[i,4:5])
      sum_3 <- rbind(sum_3, tmp)
      sum_3 <- sum_3[!duplicated(sum_3[,1:2]),]
    }
    
    if(first_result[i,3] == 5) {
      tmp <- rbind(first_result[i,1:2],
                   first_result[i,4:5])
      sum_5 <- rbind(sum_5, tmp)
      sum_5 <- sum_5[!duplicated(sum_5[,1:2]),]
    }
    
    if(first_result[i,3] == 10) {
      tmp <- rbind(first_result[i,1:2],
                   first_result[i,4:5])
      sum_10 <- rbind(sum_10, tmp)
      sum_10 <- sum_10[!duplicated(sum_10[,1:2]),]
    }
  }
  
  print(sum_1)
  print("cluster 1 new centriod: ")
  print(paste(mean(sum_1[,1]), mean(sum_1[,2])))
  
  print(sum_2)
  print("cluster 2 new centriod: ")
  print(paste(mean(sum_2[,1]), mean(sum_2[,2])))
  
  print(sum_3)
  print("cluster 3 new centriod: ")
  print(paste(mean(sum_3[,1]), mean(sum_3[,2])))
  
  print(sum_5)
  print("cluster 5 new centriod: ")
  print(paste(mean(sum_5[,1]), mean(sum_5[,2])))
  
  print(sum_10)
  print("cluster 10 new centriod: ")
  print(paste(mean(sum_10[,1]), mean(sum_10[,2])))
  
}

euclidean_dist <- function(p1, p2) {
  dist = sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(dist)
}

# main()
library("calibrate")
library("geosphere")
library("hash")
week5_5B_q1()
