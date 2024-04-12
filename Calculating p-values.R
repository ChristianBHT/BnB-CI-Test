
set.seed(1984)
# Normal data Null is true
data <- normal_data(1000)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- 1-pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

data <- normal_data(1000)
output2 <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output2[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output2)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 20)

# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))
pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r


data <- exponential_adjusted(1000)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 20)

# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))
pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

data <- exponential_adjusted(1000)
output2 <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output2[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output2)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 20)

# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))
pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

data <- poisson_adjusted(1000)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 30, col = 'lightblue', main = "Null Distribution", xlab = ('RMSE'))
# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))
pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

data <- poisson_adjusted(1000)
output2 <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output2[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output2)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 20)
hist(as.numeric(NullDist$X2), breaks = 20)

# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))

pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r


data <- non_lin_normal(1000)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  # Calculate progress 
  cat(sprintf("Drawing from Null: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 30, col = 'lightblue', main = "Null Distribution", xlab = ('RMSE'))
# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))
pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

data <- non_lin_normal(1000)
output2 <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output2[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  # Calculate progress 
  cat(sprintf("Drawing from null: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output2)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 20)
hist(as.numeric(NullDist$X2), breaks = 20)

# Do 30 tests
rmse <- list()
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
  rmse[i] <- test[[1]]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(NullDist$X1))/sd(NullDist$X1)
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(NullDist$X2))/sd(NullDist$X2)
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_rmse <- mean(unlist(p_value1))
pseudo_pvalues_r2 <- mean(unlist(p_value2))
pseudo_pvalues_rmse_para <- mean(unlist(param_p_value1))
pseudo_pvalues_r2_para <- mean(unlist(param_p_value2))

pseudo_pvalues_rmse 
pseudo_pvalues_r2 
pseudo_pvalues_rmse_para 
pseudo_pvalues_r2_para 

cond_var <- data.frame(data$X2)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r





