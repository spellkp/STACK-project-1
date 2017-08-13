A <- read.csv("Combined_A.csv")
B <- read.csv("Combined_B.csv")
C <- read.csv("Combined_C.csv")

for (i in 2:ncol(B)) {
  
  #print(summary(B))
  
  low <- min(B[,2:ncol(B)]) #when using [,2:ncol(A)] gives the same number
  print(low)
  
  q <- quantile(B$JEC, prob = c(0.25, 0.5, 0.75)) #only for column JEC
  print(q)
  
  high <- max(B[,2:ncol(B)])
  print(high)

}

