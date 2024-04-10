rm(list = ls())
library(ipred)
library(caret)
library(Metrics)
library(parallel)
library(pbapply)
library(xgboost)
library('GeneralisedCovarianceMeasure')
############################ Simulations ############################ 
  
  
  
cl <- makeCluster(detectCores()-1, type = "PSOCK")


for (N in c(100, 500, 1000, 1500, 2000, 3000)){
  p = 0.8
  R <- 1000 
  no_tests <- 50 
  seed <- N # Set seed for reproducibility
  
  output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
  
  clusterExport(cl,  varlist=c('normal_data', 'CItest_xgboost', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
  clusterEvalQ(cl, c(library('caret'),
                     library('Metrics'),
                     library('dplyr'),
                     library('xgboost'),
                     library('GeneralisedCovarianceMeasure')))
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    local_seed <- seed + i # Update seed for each test
    set.seed(local_seed)
    
    data <- normal_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- CItest_xgboost(data = data, formula = X4 ~ X3 + X1 + X2, p = p,  indices = NULL)
    }
    
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
  })
  # Save the output_matrix to a file
  filename <- paste0("output_matrix_N_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores()-1, type = "PSOCK")
for (N in c(100, 500, 1000, 1500, 2000, 3000)){
  p = 0.8
  R <- 1000 
  no_tests <- 50 
  seed <- N # Set seed for reproducibility
  
  output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
  
  clusterExport(cl,  varlist=c('non_lin_fork', 'CItest_xgboost', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
  clusterEvalQ(cl, c(library('caret'),
                     library('Metrics'),
                     library('dplyr'),
                     library('xgboost'),
                     library('GeneralisedCovarianceMeasure')))
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    local_seed <- seed + i # Update seed for each test
    set.seed(local_seed)
    
    data <- non_lin_fork(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- CItest_xgboost(data = data, formula = X3 ~ X2 + X1, p = p,  indices = NULL)
    }
    
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
  })
  # Save the output_matrix to a file
  filename <- paste0("non_lin_fork_true_output_N_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores()-1, type = "PSOCK")
for (N in c(100, 500, 1000, 1500, 2000, 3000)){
  p = 0.8
  R <- 1000 
  no_tests <- 50 
  seed <- N # Set seed for reproducibility
  
  output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
  
  clusterExport(cl,  varlist=c('poisson_adjusted', 'CItest_xgboost', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
  clusterEvalQ(cl, c(library('caret'),
                     library('Metrics'),
                     library('dplyr'),
                     library('xgboost'),
                     library('GeneralisedCovarianceMeasure')))
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    local_seed <- seed + i # Update seed for each test
    set.seed(local_seed)
    
    data <- poisson_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- CItest_xgboost(data = data, formula = X4 ~ X3 + X2 + X1, p = p,  indices = NULL)
    }
    
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
  })
  # Save the output_matrix to a file
  filename <- paste0("poisson_adjusted_true_output_N_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

cl <- makeCluster(detectCores()-1, type = "PSOCK")
for (N in c(100, 500, 1000, 1500, 2000, 3000)){
  p = 0.8
  R <- 1000 
  no_tests <- 50 
  seed <- N # Set seed for reproducibility
  
  output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
  
  clusterExport(cl,  varlist=c('uniform_noise', 'CItest_xgboost', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
  clusterEvalQ(cl, c(library('caret'),
                     library('Metrics'),
                     library('dplyr'),
                     library('xgboost'),
                     library('GeneralisedCovarianceMeasure')))
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    local_seed <- seed + i # Update seed for each test
    set.seed(local_seed)
    
    data <- uniform_noise(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- CItest_xgboost(data = data, formula = X4 ~ X3 + X2 + X1, p = p,  indices = NULL)
    }
    
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
  })
  # Save the output_matrix to a file
  filename <- paste0("uniform_noise_true_output_N_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

cl <- makeCluster(detectCores()-1, type = "PSOCK")
for (N in c(100, 500, 1000, 1500, 2000, 3000)){
  p = 0.8
  R <- 1000 
  no_tests <- 50 
  seed <- N # Set seed for reproducibility
  
  output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
  
  clusterExport(cl,  varlist=c('exponential_adjusted', 'CItest_xgboost', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
  clusterEvalQ(cl, c(library('caret'),
                     library('Metrics'),
                     library('dplyr'),
                     library('xgboost'),
                     library('GeneralisedCovarianceMeasure')))
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    local_seed <- seed + i # Update seed for each test
    set.seed(local_seed)
    
    data <- exponential_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- CItest_xgboost(data = data, formula = X4 ~ X3 + X2 + X1, p = p,  indices = NULL)
    }
    
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
  })
  # Save the output_matrix to a file
  filename <- paste0("exponential_adjusted_true_output_N_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)



  