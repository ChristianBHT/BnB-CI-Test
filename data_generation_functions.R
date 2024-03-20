
# The fork
normal_fork <- function(N){
  X1 = rnorm(N,0,1)
  X2 = rnorm(N,X1,1)
  X3 = rnorm(N,X1,1)
  df <- data.frame(X1, X2, X3)
  return(df)
}

# The fork non-linear
non_lin_fork <- function(N){
  X1 = rnorm(N,1,1)
  X2 = cos(X1)  + rnorm(N,0,0.1)
  X3 = log(abs(X1)) + rnorm(N,0,0.5)
  df <- data.frame(X1,X2,X3)
  return(df)
}


uniform_noise <- function(N) {
  X1 = rnorm(N, 1, 1)
  X2 = rnorm(N, 0, 1)
  X3_mean = X2 + X1 + X2 * X1
  X4_mean = X2 + X1 + X2 * X1
  X3 = X3_mean + runif(N, min=-2, max=2)  
  X4 = X4_mean + runif(N, min=-2, max=2)  
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

model5_exponential_adjusted <- function(N) {
  X1 = rnorm(N, 1, 1)
  X2 = rnorm(N, 0, 1)
  X3_mean = X2 + X1 + X2 * X1
  X4_mean = X2 + X1 + X2 * X1
  rate_param = 1
  X3_errors = rexp(N, rate = rate_param) - (1 / rate_param)
  X4_errors = rexp(N, rate = rate_param) - (1 / rate_param)
  X3 = X3_mean + X3_errors
  X4 = X4_mean + X4_errors
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

non_lin_norm2 <- function(N){
  X1 = rnorm(N,1,1)
  X2 = rnorm(N,0,1)
  X3 = rnorm(N,exp(X2*X1),1)
  X4 = rnorm(N,X2*X1,1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}


non_lin_norm3 <- function(N){
  X1 = rnorm(N,1,1)
  X2 = X1 + rnorm(N,0,1)
  X3 = X1*X2 + rnorm(N,0,1)
  X4 = X1 + X2 + X1*X2 + rnorm(N,0,1)
  X5 = X3 + X4 + rnorm(N,0,1)
  df <- data.frame(X1, X2, X3, X4, X5)
  return(df)
}

diff_data_types <- function(N) {
  X1 <- rnorm(N)
  X2 <- rnorm(N,exp(X1),1) 
  x3b1 <- X1 + X2 - X1*X2
  x3b2 <- X1 + X2 + X1*X2
  x3p1 <- 1/(1+exp(x3b1) + exp(x3b2))
  x3p2 <- exp(x3b1) /(1+exp(x3b1) + exp(x3b2))
  random <- runif(N,0, 1)
  X3 <- ifelse(random < x3p1, 1, ifelse(random < x3p1 + x3p2,2,3))
  X4 <- X1 - X2 + X1*X2  + rnorm(N)
  x5b1 = X4 - X3  
  x5p1 = 1/(1+exp(x5b1))
  random = runif(N,0,1)
  X5 = ifelse(random < x5p1, 0, 1)
  df <- data.frame(X1,X2,X3,X4,X5)
  hist(X5)
  return(df)
}


