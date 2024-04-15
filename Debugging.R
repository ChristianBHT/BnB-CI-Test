# Bugs and testing script.
# Testing example
data <- non_lin_normal(1000)

output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, nthread = 1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Drawing from null: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null Distribution")
hist(as.numeric(unlist(NullDist[2])), col = 'black', main = "Null Distribution")
hist(data$X3)
hist(data$X4) #Choose the variable which have the most symmetric distribution as your dependent

# Do 30 tests
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(unlist(NullDist[1])))/sd(unlist(NullDist[1]))
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_acc <- mean(unlist(p_value1))
pseudo_pvalues_kappa <- mean(unlist(p_value2))
pseudo_pvalues_acc_para <- mean(unlist(param_p_value1))
pseudo_pvalues_kappa_para <- mean(unlist(param_p_value2))

pseudo_pvalues_acc 
pseudo_pvalues_kappa 
pseudo_pvalues_acc_para 
pseudo_pvalues_kappa_para 

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

# Binary outcome
set.seed(1990)
data <- diff_data_types(1000)

output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, nthread = 1, p = 0.85, objective = "binary:logistic")
  # Calculate progress 
  cat(sprintf("Drawing from null: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null Distribution")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null Distribution")

# Do 30 tests
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85, objective = "binary:logistic")
  
  p_value1[i] <- (sum(NullDist[1] >= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(unlist(NullDist[1])))/sd(unlist(NullDist[1]))
  param_p_value1[i] <- 1-pnorm(Z1)
  Z2 <- (test[[2]]-mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_1 <- mean(unlist(p_value1))
pseudo_pvalues_2 <- mean(unlist(p_value2))
pseudo_pvalues_1_para <- mean(unlist(param_p_value1))
pseudo_pvalues_2_para <- mean(unlist(param_p_value2))

pseudo_pvalues_1 
pseudo_pvalues_2 
pseudo_pvalues_1_para 
pseudo_pvalues_2_para 

cond_var <- data.frame(data$X2, data$X1)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

#Switching to a categorical
set.seed(1990)
data <- diff_data_types(1000)
output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = X3 ~ X4 + X2, nthread = 1, p = 0.85, objective = "multi:softmax", num_class = 4)
  # Calculate progress 
  cat(sprintf("Drawing from null: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null Distribution")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null Distribution")

# Do 30 tests
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
for (i in 1:30) {
  test <- TestGenerator(data = data, formula = X3 ~ X4 + X2, nthread = 1, p = 0.85, objective = "multi:softmax", num_class = 4)
  
  p_value1[i] <- (sum(NullDist[1] >= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(unlist(NullDist[1])))/sd(unlist(NullDist[1]))
  param_p_value1[i] <- 1-pnorm(Z1)
  Z2 <- (test[[2]]-mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}
# Calculate pseudo p-values
pseudo_pvalues_1 <- mean(unlist(p_value1))
pseudo_pvalues_2 <- mean(unlist(p_value2))
pseudo_pvalues_1_para <- mean(unlist(param_p_value1))
pseudo_pvalues_2_para <- mean(unlist(param_p_value2))

pseudo_pvalues_1 
pseudo_pvalues_2 
pseudo_pvalues_1_para 
pseudo_pvalues_2_para 

cond_var <- data.frame(data$X2)
gcm_test_r <- gcm.test(data$X3, data$X4, Z = cond_var)
gcm_test_r

#Switching to a categorical
set.seed(1990)
data <- random_Z_effects(1000, Zs = 10)
cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6)
gcm.test(data$X, data$Y, Z = cond_var)

output <- list()
R = 1000
# Generate a null distribution with Monte Carlo Cross Validation
for (i in 1:R) {
  output[[i]] <- NullGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6, nthread = 1, p = 0.85)
  # Calculate progress 
  cat(sprintf("Drawing from null: %d\r", i))
  flush.console()
}

output_df <- do.call(rbind, output)
NullDist <- data.frame(output_df)
hist(as.numeric(unlist(NullDist[1])), col = 'lightblue', main = "Null Distribution")
hist(as.numeric(unlist(NullDist[2])), col = 'white', main = "Null Distribution")

# Do 30 tests
p_value1 <- list()
p_value2 <- list()
param_p_value1 <- list()
param_p_value2 <- list()
rmse <- list()

for (i in 1:100) {
  test <- TestGenerator(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6, nthread = 1, p = 0.85)
  rmse[i] <- test[1]
  p_value1[i] <- (sum(NullDist[1] <= test[[1]])+1) / (nrow(NullDist)+1)
  p_value2[i] <- (sum(NullDist[2] >= test[[2]])+1) / (nrow(NullDist)+1)
  Z1 <- (test[[1]]-mean(unlist(NullDist[1])))/sd(unlist(NullDist[1]))
  param_p_value1[i] <- pnorm(Z1)
  Z2 <- (test[[2]]-mean(as.numeric(unlist(NullDist[2]))))/sd(as.numeric(unlist(NullDist[2])))
  param_p_value2[i] <- 1-pnorm(Z2)
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  # Sys.sleep(0.1)
}
hist(unlist(rmse))
mean(unlist(rmse))
mean(as.numeric(unlist(NullDist[1])))

# Calculate pseudo p-values
pseudo_pvalues_1 <- mean(unlist(p_value1))
pseudo_pvalues_2 <- mean(unlist(p_value2))
pseudo_pvalues_1_para <- mean(unlist(param_p_value1))
pseudo_pvalues_2_para <- mean(unlist(param_p_value2))

pseudo_pvalues_1 
pseudo_pvalues_2 
pseudo_pvalues_1_para 
pseudo_pvalues_2_para 




