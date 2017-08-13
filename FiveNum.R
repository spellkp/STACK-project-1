
A <- read.csv("Combined_A.csv")

##summary(A)

for(i in 2:ncol(A)) {
  
  low <- min(A[,1:ncol(A)])
  print(low)

  q <- quantile(A[,1:ncol(A)])
  print(q)
  
  ##quan1 <- quantile(A[,2], c(0.25, 0.50, 0.75))
  ##print(quan1)
  ##quan2 <- quantile(A[,3], c(0.25, 0.50, 0.75))
  ##print(quan2)
  ##quan3 <- quantile(A[,4], c(0.25, 0.50, 0.75))
  ##print(quan3)
  
  quan <- quantile (A[,1:ncol(A)], 0.25)
  print(quan)
  
  high <- max(A[,1:ncol(A)])
  print(high)
  
  
}
