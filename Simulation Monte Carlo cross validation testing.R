library(parallel)
library(pbapply) 
library(GeneralisedCovarianceMeasure)
library(CondIndTests)
DAG <- 'dag {
  bb="0,0,1,1"
  X1 [pos="0.027,0.039"]
  X2 [pos="0.029,0.157"]
  X3 [pos="0.123,0.041"]
  X4 [pos="0.124,0.157"]
  X1 -> X3
  X1 -> X4
  X2 -> X3
  X2 -> X4
}'

################ Normal Data ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'normal_data',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })

results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- normal_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
  
    return(data.frame(
      test_number = i,
      data_generation = "normal_data",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("normal_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'normal_data',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- normal_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "normal_data",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("normal_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################


################ Non linear Normal Data ######################



cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'non_lin_normal',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- non_lin_normal(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    # p_values
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "non_lin_normal",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("non_lin_normal_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'non_lin_normal',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- non_lin_normal(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "non_lin_normal",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("non_lin_normal_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################





################ Uniform Noise Data ######################



cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'uniform_noise',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- uniform_noise(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "uniform_noise",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("uniform_noise_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'uniform_noise',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- uniform_noise(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "uniform_noise",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("uniform_noise_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################


################ Exponential noise Data ######################



cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'exponential_adjusted',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- exponential_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "exponential_adjusted",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("exponential_adjusted_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'exponential_adjusted',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- exponential_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "exponential_adjusted",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("exponential_adjusted_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################






################ Poisson noise Data ######################



cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'poisson_adjusted',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- poisson_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "poisson_adjusted",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("poisson_adjusted_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'poisson_adjusted',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- poisson_adjusted(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "poisson_adjusted",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("poisson_adjusted_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Skewed Data ######################



cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'skewed_data',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- skewed_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "skewed_data",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("skewed_data_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'skewed_data',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests',
                                'DAG'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(DescTools)
    library(GeneralisedCovarianceMeasure)
    library(dagitty)
    library(CondIndTests)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- skewed_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "skewed_data",  
      CI_statement = "X4 _||_ X3 | X2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      param_pvalue1 = p_values[3],
      param_pvalue2 = p_values[4],
      GCM_pvalue = gcm_test$p.value, 
      KCI_pvalue = kci_test$pvalue,
      local_test = local_test
    ))
  })
  
  # Save output
  filename <- paste0("skewed_data_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

































