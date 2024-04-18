
set.seed(1983)
obs = 1000
# First out normal data Null is true
data <- normal_data(obs)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Normal Data Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Normal Data Null is True (R2)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}



cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


Initial_test_res <- data.frame(
  data_generation = "normal_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)


data <- normal_data(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Normal Data Null is False (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Normal Data Null is False (MSE)")

# Do 30 tests
test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)


cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "normal_data",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)
# SECOND CASE exponential_adjusted

data <- exponential_adjusted(obs)
output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Normal Data Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Normal Data Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)


cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "exponential_adjusted",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

data <- exponential_adjusted(obs)
output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "exponential_adjusted",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)

# Case non lin normal
data <- non_lin_normal(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "non_lin_normal",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

data <- non_lin_normal(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "non_lin_normal",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res
# poisson adjusted
data <- poisson_adjusted(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "poisson_adjusted",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

data <- poisson_adjusted(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "poisson_adjusted",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res
# Uniform noise
data <- uniform_noise(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "uniform_noise",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

data <- uniform_noise(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "uniform_noise",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

# binary_data
data <- binary_data(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "binary:logistic")
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True ")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "binary:logistic")
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "binary_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

data <- binary_data(obs)

output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "binary:logistic")
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "binary:logistic")
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "binary_data",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

# binary_data
data <- binary_data_2(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "binary:logistic")
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True ")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "binary:logistic")
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test

new_row <- data.frame(
  data_generation = "binary_data_2",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

data <- binary_data_2(obs)

output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "binary:logistic")
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "binary:logistic")
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test

new_row <- data.frame(
  data_generation = "binary_data_2",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

# catgorical_data
data <- diff_data_types(obs)
output <- list()

for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "multi:softprob", num_class = 4)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True ")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "multi:softprob", num_class = 4)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "diff_data_types",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

data <- diff_data_types(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "multi:softprob", num_class = 4)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue')
hist(as.numeric(unlist(NullDist[2])), col = 'white')

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85, objective = "multi:softprob", num_class = 4)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) >= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


new_row <- data.frame(
  data_generation = "diff_data_types",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

# Skewed data
data <- skewed_data(obs)
output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
# hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
# hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) #RMSE, how much of distribution below test
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) #MSE, how much of distribution above test

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)


cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "skewed_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <-rbind(Initial_test_res, new_row)

data <- skewed_data(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)

# Do 30 tests
test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+0.1) / (nrow(NullDist)+0.1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+0.1) / (nrow(NullDist)+0.1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)


cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "skewed_data",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)

# switching_regression_data data
data <- switching_regression_data(obs)
output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) #RMSE, how much of distribution below test
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) #MSE, how much of distribution above test

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)


cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "switching_regression_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <-rbind(Initial_test_res, new_row)

data <- switching_regression_data(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is false (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is false (MSE)")

# Do 30 tests
test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+0.1) / (nrow(NullDist)+0.1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+0.1) / (nrow(NullDist)+0.1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)


cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "switching_regression_data",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)



# hierarchical_data data
data <- hierarchical_data(obs)
output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True (MSE)")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1) #RMSE, how much of distribution below test
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) #MSE, how much of distribution above test

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)


cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "hierarchical_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <-rbind(Initial_test_res, new_row)

data <- hierarchical_data(obs)
output <- list()
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is false (RMSE)")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is false (MSE)")

# Do 30 tests
test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+0.1) / (nrow(NullDist)+0.1) 
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+0.1) / (nrow(NullDist)+0.1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- pnorm(Z2)


cond_var <- data.frame(data$X2)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)

new_row <- data.frame(
  data_generation = "hierarchical_data",  
  CI_statement = "X4 _||_ X3 |  X2",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
Initial_test_res <- rbind(Initial_test_res, new_row)

# ZZZZZZssss

data <- random_Z_effects(obs, Zs = 20)

output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True ")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18, data$Z19, data$Z20)
gcm_test <- gcm.test(data$Y, data$X, Z = cond_var)

new_row <- data.frame(
  data_generation = "Random_Z_20",  
  CI_statement = "Y _||_ X |  Z1,..., Z20",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 , p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is false")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is false")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 , p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18)
gcm_test <- gcm.test(data$Y, data$X, Z = cond_var)

new_row <- data.frame(
  data_generation = "Random_Z_20",  
  CI_statement = "Y _||_ X |  Z1,..., Z18",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)




data <- random_Z_effects(obs, Zs = 10)

output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is True")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is True ")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10)
gcm_test <- gcm.test(data$Y, data$X, Z = cond_var)

new_row <- data.frame(
  data_generation = "Random_Z_10",  
  CI_statement = "Y _||_ X |  Z1,..., Z10",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)

output <- list()
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8, p = 0.85)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null is false")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null is false")

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 , p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_value1 <- (sum(as.numeric(unlist(NullDist[1])) <= mean(unlist(test1)))+1) / (nrow(NullDist)+1)
p_value2 <- (sum(as.numeric(unlist(NullDist[2])) <= mean(unlist(test2)))+1) / (nrow(NullDist)+1) 

Z1 <- (mean(unlist(test1)) - mean(as.numeric(unlist(NullDist[1]))))/sd(as.numeric(unlist(NullDist[1])))
param_p_value1 <- 1-pnorm(Z1)

Z2 <- (mean(unlist(test2)) - mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
param_p_value2 <- 1-pnorm(Z2)

cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8)
gcm_test <- gcm.test(data$Y, data$X, Z = cond_var)

new_row <- data.frame(
  data_generation = "Random_Z_10",  
  CI_statement = "Y _||_ X |  Z1,..., Z8",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)

Initial_test_res <- rbind(Initial_test_res, new_row)
Initial_test_res

