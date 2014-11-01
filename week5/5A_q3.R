
week5_5A_q3 <- function() {
  
  # initial representative points
  x <- c(0,0)
  y <- c(10,10)
  
  a <- c(1,6)
  b <- c(3,7)
  c <- c(4,3)
  d <- c(7,7)
  e <- c(8,2)
  f <- c(9,5)
  
  # show chart
  plot(rbind(x[1],y[1]),
       rbind(x[2],y[2]),
       type="p",
       xlab="X-axis",ylab="y-axix",asp=1,
       xlim=c(0, 20),ylim=c(0, 20),
       pch="*",col="black",bg="grey")
  points(rbind(a[1],b[1],c[1],d[1],e[1],f[1]), 
         rbind(a[2],b[2],c[2],d[2],e[2],f[2]),
         type="p",pch=16,col="blue")
  # draw point label
  allpoints <- rbind(x,y,a,b,c,d,e,f)
  textxy(allpoints[,1],allpoints[,2],offset=0.7,cex=0.6,
         labs=c(paste(paste("(",allpoints[,1],","),allpoints[,2],")")))
  
  # select five points
  r <- rbind(x,y)
  colnames(r) <- c("x", "y")
  candidates <- rbind(a,b,c,d,e,f)
  for(i in seq(from=1, to=5, by=1)) {
    writeLines(paste("Round ", i))
    
    result<- compute_max_dist(r,candidates)
    furthest_point <- result[[1]]
    r <- rbind(r,furthest_point)
    candidates <- subset(candidates,
                         !(candidates[,1] == furthest_point[1] &
                         candidates[,2] == furthest_point[2]),)
    print(candidates)
    writeLines("\n\n")
    
    # e -> b -> c -> d -> f -> a
  }
  
  
}

# Find the point whose minimum distance to any of the previously selected points is maximum
compute_max_dist <- function(r,candidates) {
  
  furthest_point <- NULL
  global_max_dist <- 0
  for(i in seq(from=1, to=nrow(candidates), by=1)) {
    candicate <- candidates[i,]
    
    min_dist <- -999
    for(j in seq(from=1, to=nrow(r), by=1)) {
      rpoint <- r[j,]
      
      dist <- euclidean_dist(candicate,rpoint)
      
      if(j == 1) {
        min_dist <- dist
      }
      else {
        if(min_dist > dist) {
          min_dist <- dist
        }
      }
    }
    
    print(paste(paste("min_dist to ", 
                      paste(candicate[1], ",", candicate[2]), ":"), min_dist))
    
    if(global_max_dist < min_dist) {
      global_max_dist = min_dist
      furthest_point = candicate
    }
  }
  
  print(paste(paste("furthest point: ", 
                    paste(furthest_point[1], ",", furthest_point[2]), " => "), global_max_dist))
  
  result <- list(furthest_point,global_max_dist)
  return(result)
}

euclidean_dist <- function(p1, p2) {
  dist = sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(dist)
}

# main()
library("calibrate")
week5_5A_q3()
