poisson_adjusted_true_output_N_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/poisson_adjusted_true_output_N_1000.rds")
#Test 1 
data_frames <- vector("list", 50)
p_values <- numeric(50)
for (i in 1:50) {
  comp_ci <- poisson_adjusted_true_output_N_1000[[i]]$Boot_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- poisson_adjusted_true_output_N_1000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:50) {
  rmse_data <- data_frames[[i]]$RMSE
  
  hist(rmse_data, main="Histogram of RMSE", xlab="RMSE", col="yellow", border="blue", breaks = 40)
  p_value <- p_values[i]
  text(x = max(rmse_data, na.rm = TRUE) * 0.8, y = 40, 
       labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       col = "red", cex = 1.2)
  
}
# p-verdi <- antall under null/total
poisson_adjusted_true_output_N_2000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/poisson_adjusted_true_output_N_2000.rds")

data_frames <- vector("list", 50)
p_values <- numeric(50)
for (i in 1:50) {
  comp_ci <- poisson_adjusted_true_output_N_2000[[i]]$Boot_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- poisson_adjusted_true_output_N_2000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:50) {
  rmse_data <- data_frames[[i]]$RMSE
  
  hist(rmse_data, main="Histogram of RMSE", xlab="RMSE", col="gray", border="blue", breaks = 40)
  p_value <- p_values[i]
  text(x = max(rmse_data, na.rm = TRUE) * 0.8, y = 40, 
       labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       col = "red", cex = 1.2)
  
}


non_lin_fork_true_output_N_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/non_lin_fork_true_output_N_1000.rds")

data_frames <- vector("list", 50)
p_values <- numeric(50)
for (i in 1:50) {
  comp_ci <- non_lin_fork_true_output_N_1000[[i]]$Boot_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- non_lin_fork_true_output_N_1000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:50) {
  rmse_data <- data_frames[[i]]$RMSE
  
  hist(rmse_data, main="Histogram of RMSE", xlab="RMSE", col="blue", border="blue", breaks = 40)
  p_value <- p_values[i]
  text(x = max(rmse_data, na.rm = TRUE) * -0.8, y = 40, 
       labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       col = "red", cex = 1.2)
  
}

normal_data_false_output_N_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/normal_data_false_output_N_1000.rds")
data_frames <- vector("list", 50)
p_values <- numeric(50)
for (i in 1:50) {
  comp_ci <- normal_data_false_output_N_1000[[i]]$Boot_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- normal_data_false_output_N_1000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:50) {
  rmse_data <- data_frames[[i]]$RMSE
  
  hist(rmse_data, main="Histogram of RMSE", xlab="RMSE", col="red", border="blue", breaks = 30)
  p_value <- p_values[i]
  text(x = max(rmse_data, na.rm = TRUE) * -0.8, y = 40, 
       labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       col = "red", cex = 1.2)
  
}

normal_data_true_output_N_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/normal_true_output_N_1000.rds")
data_frames <- vector("list", 50)
p_values <- numeric(50)
for (i in 1:50) {
  comp_ci <- normal_data_true_output_N_1000[[i]]$Comp_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- normal_data_true_output_N_1000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:50) {
  rmse_data <- data_frames[[i]]$'RMSE'
  p_value_compu <- sum(rmse_data>0)/1000
  hist(rmse_data, main="Difference RMSE", xlab="RMSE", col="white", border="blue", breaks = 30)
  p_value <- p_values[i]
  
  text(x = max(rmse_data, na.rm = TRUE) * 0.8, y = 40, 
       labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       col = "red", cex = 1.2)
  
}

pValuesComp <- numeric(50)
pValuesGCM <- numeric(50)
for (i in 1:50) {
  rmse_data <- data_frames[[i]]$'RMSE'
  pValuesComp[[i]] <- sum(rmse_data>0)/1000
  pValuesGCM[[i]] <- p_values[i]
  
  
}
hist(pValuesComp, xlim = range(0,1))
hist(pValuesGCM, xlim = range(0,1))

uniform <- runif(50, min=0, max=1)
hist(uniform)


spesial_case_test_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/spesial_case_test_1000.rds")
data_frames <- vector("list", 200)
p_values <- numeric(200)
for (i in 1:200) {
  comp_ci <- spesial_case_test_1000[[i]]$Comp_CI
  matrix_data <- do.call(rbind, comp_ci)
  test_comp <- as.data.frame(matrix_data)
  names(test_comp) <- c("RMSE", "R-squared")
  data_frames[[i]] <- test_comp
  gcm_p_value_1 <- spesial_case_test_1000[[i]]$gcm_test_p
  p_values[i] <- gcm_p_value_1
}

for (i in 1:200) {
  r2_data <- data_frames[[i]]$'R-squared'
  p_value_compu <- sum(rmse_data>0)/1000
  # hist(rmse_data, main="Difference RMSE", xlab="RMSE", col="white", border="blue", breaks = 30)
  p_value <- p_values[i]
  
  # text(x = max(rmse_data, na.rm = TRUE) * 0.8, y = 40, 
       # labels = paste("GCM p-value:", format(p_value, digits = 4)), 
       # col = "red", cex = 1.2)
  
}
hist(r2_data)
pValuesComp <- numeric(200)
pValuesGCM <- numeric(200)
for (i in 1:200) {
  r2_data <- data_frames[[i]]$'RMSE'
  pValuesComp[[i]] <- sum(r2_data>0)/200
  pValuesGCM[[i]] <- p_values[i]
  
  
}
Pvalues <- data.frame(PV = pValuesComp - 0.5)
summary(lm(data = Pvalues, formula = PV ~ 1))

hist(pValuesComp, breaks = 50, xlim = range(0,1))
hist(pValuesGCM, breaks = 50, xlim = range(0,1))

hist(rmse_data)