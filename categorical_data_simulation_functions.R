
multinominal <- function(N, zeta = 1) {
  Z1 <- rnorm(N)
  
  Z2 <- rnorm(N) 
  
  xb1 <- Z2 + zeta*Z1*Z2 + zeta*Z1  
  
  xb2 <- Z2 - zeta*Z1  
  
  xp1 <- 1/(1+exp(xb1) + exp(xb2))
  xp2 <- exp(xb1) /(1+exp(xb1) + exp(xb2))
  random <- runif(N,0, 1)
  X <- ifelse(random < xp1, "C", ifelse(random < xp1 + xp2,"A","B"))
  
  yb1 = zeta*Z1*Z2 
  yb2 <- exp(Z2) +  zeta*Z1  
  
  yp1 <- 1/(1+exp(yb1) + exp(yb2))
  yp2 <- exp(yb1) /(1+exp(yb1) + exp(yb2))
  random <- runif(N,0, 1)
  Y <- ifelse(random < yp1, "X", ifelse(random < yp1 + yp2,"Y","Z"))
  
  df <- data.frame(Z1,Z2,X,Y)
  
  return(df)
}


CategorizeInteractiondData <- function(N) {
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  X3 <- numeric(N)  
  X4 <- numeric(N)  
  
  for (i in 1:N) {
    if (X1[i] < 0 && X2[i] < 0) {
      X3[i] <- 0
    } else if (X1[i] < 0 && X2[i] >= 0) {
      X3[i] <- 1
    } else if (X1[i] >= 0 && X2[i] < 0) {
      X3[i] <- 2
    } else {
      X3[i] <- 3
    }
    
    if (X1[i] + X2[i] < -1) {
      X4[i] <- 0
    } else if (X1[i] + X2[i] < 0) {
      X4[i] <- 1
    } else if (X1[i] + X2[i] < 1) {
      X4[i] <- 2
    } else {
      X4[i] <- 3
    }
  }
  
  data_frame <- data.frame(X1, X2, X3, X4)
  
  return(data_frame)
}

