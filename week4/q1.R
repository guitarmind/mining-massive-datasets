
week4_q1 <- function() {
  
  # create rating matrix
  data <- rbind(c(1,2,3,4,5),
                c(2,3,2,5,3),
                c(5,5,5,3,2))
  rownames(data) <- c("A", "B", "C")
  colnames(data) <- c("M", "N", "P", "Q", "R")
  print(data)
  
  # row averages
  rowAvg <- rowMeans(data)
  print('Row averages:')
  print(rowAvg)
  #   print(paste('Row averages = ', rowAvg))
  
  #   print(paste('Column averages = ', colAvg))
  norm <- data - rowAvg
  print(norm)
  
  # column averages
  colAvg <- colMeans(norm)
  print('Column averages:')
  print(colAvg)
  
  print(class(colAvg))
  print(typeof(colAvg))
  
  norm <- t(t(norm) - colAvg)
  print(norm)
  
  smallest <- which(norm == min(norm), arr.ind = TRUE)
  print('Smallest element:')
  print(smallest)
  
  largest <- which(norm == max(norm), arr.ind = TRUE)
  print('Largest element:')
  print(largest)
  
}

# main()
library("Matrix")
week4_q1()
