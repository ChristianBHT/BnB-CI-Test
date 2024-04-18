#Normal simple data
normal_data <- function(N){
  X1 <- rnorm(N,0,1)
  X2 <- rnorm(N,0,1)
  X3 <- rnorm(N,X1 + X2,1)
  X4 <- rnorm(N,X1 + X2,1)
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

# The fork
# normal_fork <- function(N){
#   X1 = rnorm(N,0,1)
#   X2 = rnorm(N,X1,1)
#   X3 = rnorm(N,X1,1)
#   df <- data.frame(X1, X2, X3)
#   return(df)
# }
# 

hierarchical_data <- function(N, groups = 4) {
  group <- rep(1:groups, each = N/groups)
  X1 <- rnorm(N) + group
  X2 <- rnorm(N, mean = group)
  X3 <- X1 * X2 + group + rnorm(N)
  X4 <- X1 - X2 + group + rnorm(N)
  df <- data.frame(Group = group, X1, X2, X3, X4)
  return(df)
}

skewed_data <- function(N) {
  X1 <- rexp(N, rate = 0.5)
  X2 <- rexp(N, rate = 0.5)
  X3 <- X1 + X2 + rnorm(N)
  X4 <- X1 * X2 + rnorm(N)
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

switching_regression_data <- function(N) {
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  regime <- rbinom(N, 1, 0.5)  # Binary indicator for regime
  X3 <- ifelse(regime == 1, X1 + X2 + rnorm(N), X1 - X2 + rnorm(N))
  X4 <- ifelse(regime == 1, X1 * 0.5 + rnorm(N), X1 * 2 + rnorm(N))
  df <- data.frame(X1, X2, Regime = regime, X3, X4)
  return(df)
}

non_lin_normal <- function(N){
  X1 = rnorm(N,0,1)
  X2 = rnorm(N,0,1)
  X3 = exp(X1*X2) + rnorm(N,0,1)
  X4 <- X1*X2 + rnorm(N,0,1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}

# Four variable DAG interaction with uniform error
uniform_noise <- function(N) {
  X1 = rnorm(N, 0, 1)
  X2 = rnorm(N, 0, 1)
  X3 = X2 + X1 + X2 * X1 + runif(N, min=-2, max=2) 
  X4 = X2 + X1 + X2 * X1 + runif(N, min=-2, max=2)
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

# Four variable DAG interaction with exponential error
exponential_adjusted <- function(N, rate_param = 1) {
  X1 = rnorm(N, 0, 1)
  X2 = rnorm(N, 0, 1)
  rate_param = rate_param
  X3 = X2 + X1 + X2 * X1 + rexp(N, rate = rate_param) - (1 / rate_param)
  X4 = X2 + X1 + X2 * X1 + rexp(N, rate = rate_param) - (1 / rate_param)
  df <- data.frame(X1, X2, X3, X4)
  return(df)
}

# Four variable DAG interaction with poisson error
poisson_adjusted <- function(N){
  X1 = rnorm(N,0,1)
  X2 = rnorm(N,0,1)
  X3 = X2*X1 + (rpois(N, lambda = 1)-1)
  X4 = X2*X1  + (rpois(N, lambda = 1)-1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}

binary_data <- function(N) {
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  X3_star <- X1 + X2 + X1*X2 + rnorm(N)
  X3 <- if_else(X3_star > mean(X3_star),1,0)
  X4_star <- -X1 - X2 - X1*X2 + rnorm(N)
  X4 <- if_else(X4_star < mean(X4_star),1,0)
  data_frame <- data.frame(X1, X2, X3, X4)
  
  return(data_frame)
}

binary_data_2 <- function(N) {
  X1 <- rnorm(N)
  X2 <- rnorm(N,1) 
  x3b1 <- X1 + X2 
  x3b2 <- X1 + X2 + X1*X2
  x3p1 <- 1/(1+exp(x3b1) + exp(x3b2))
  x3p2 <- exp(x3b1) /(1+exp(x3b1) + exp(x3b2))
  random <- runif(N,0, 1)
  X3 <- ifelse(random < x3p1, 0, ifelse(random < x3p1 + x3p2,1,2))
  x4b1 = 2*X1*X2  
  x4p1 = 1/(1+exp(x4b1))
  random = runif(N,0,1)
  X4 = ifelse(random < x4p1, 0, 1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}



diff_data_types <- function(N) {
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


random_Z_effects <- function(N, Zs = 10) {
  Z <- data.frame(matrix(ncol = Zs, nrow = N))
  names(Z) <- paste0("Z", 1:Zs)
  
  error_functions <- list(
    normal = function(n) rnorm(n, mean = 0, sd = 1),
    uniform = function(n) runif(n, min = -1, max = 1),
    exp_adjusted = function(n) rexp(n, rate = 1) - 1 
  )

  for (i in 1:Zs) {
    error_type <- sample(names(error_functions), 1) 
    Z[, i] <- error_functions[[error_type]](N)
  }
  
  relationship_types <- c("linear", "quadratic")
  
  X <- rep(0, N)
  Y <- rep(0, N)
  
  for (i in 1:Zs) {
    rel_type_X <- sample(relationship_types, 1)
    rel_type_Y <- sample(relationship_types, 1)
    
    if (rel_type_X == "linear") {
      X <- X + Z[,i]
    } else if (rel_type_X == "quadratic") {
      X <- X + (Z[,i]^2)
    } 
    
    if (rel_type_Y == "linear") {
      Y <- Y + Z[,i]
    } else if (rel_type_Y == "quadratic") {
      Y <- Y + (Z[,i]^2)
    } 
  }
  
  X <- X + runif(N, min = -2, max = 2)
  Y <- Y + runif(N, min = -2, max = 2)
  
  df <- data.frame(cbind(Z, X = X, Y = Y))
  
  return(df)
}
















