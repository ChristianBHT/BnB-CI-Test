library(dplyr)
library(parallel)
library(pbapply) 
library(dagitty)
library(xgboost)
library(caret)
library(Metrics)
library(DescTools)
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

for (N in c(500,1000)) {
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,]
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
  filename <- paste0("normal_local_test_false_", N, ".rds")
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] 
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
  
  filename <- paste0("non_lin_normal_local_test_false_", N, ".rds")
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] 
    
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
  
  filename <- paste0("uniform_noise_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100, 200, 500, 1000, 1500)) {
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] #test with cis.loess
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
  filename <- paste0("uniform_noise_local_test_false_", N, ".rds")
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] #test with cis.loess
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
  
   
  filename <- paste0("exponential_adjusted_local_test_false_", N, ".rds")
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
        seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] 
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
  
  filename <- paste0("poisson_adjusted_local_test_false_", N, ".rds")
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] #test with cis.loess
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
  
   filename <- paste0("skewed_data__local_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Hierarchical Data ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  

  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'hierarchical_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- hierarchical_data(N)
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
      data_generation = "hierarchical_data",  
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
  filename <- paste0("hierarchical_data_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests<- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'hierarchical_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- hierarchical_data(N)
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
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] 
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE)
    
    return(data.frame(
      test_number = i,
      data_generation = "hierarchical_data",  
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
  filename <- paste0("hierarchical_data_local_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Switching regressions Data ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'switching_regression_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- switching_regression_data(N)
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
      data_generation = "switching_regression_data",  
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
  
  filename <- paste0("switching_regression_test_", N, ".rds")
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
                                'switching_regression_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- switching_regression_data(N)
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
    # 
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    # 
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.loess", R = 250)[3,] 
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE)
    
    return(data.frame(
      test_number = i,
      data_generation = "switching_regression_data",  
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
  
   
  filename <- paste0("switching_regression_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Binary Data 1 ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100, 200, 500, 1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'binary_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- binary_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = "binary:logistic")
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'binary:logistic')
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'binary:logistic', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- InvariantTargetPrediction(data$X4, as.factor(data$X3), cond_var, verbose = FALSE, alpha = 0.05) 
    local_test <- localTests(DAG, data = data, type = "cis.pillai")[2,2] 

    
    
    return(data.frame(
      test_number = i,
      data_generation = "binary_data",  
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
  
   
  filename <- paste0("binary_data_test_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(100, 200, 500, 1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'binary_data',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- binary_data(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'binary:logistic')
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'binary:logistic')
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'binary:logistic', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- InvariantTargetPrediction(data$X4, as.factor(data$X3), cond_var, verbose = FALSE, alpha = 0.05) 
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type = "cis.pillai")[3,2] 

    return(data.frame(
      test_number = i,
      data_generation = "binary_data",  
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
  filename <- paste0("binary_data_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Binary Data 2 ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100

  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'binary_data_2',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- binary_data_2(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'binary:logistic')
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'binary:logistic')
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'binary:logistic', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- InvariantTargetPrediction(data$X4, as.factor(data$X3), cond_var, verbose = FALSE, alpha = 0.05) 
    local_test <- localTests(DAG, data = data, type="cis.pillai")[2,] 
    
    
    return(data.frame(
      test_number = i,
      data_generation = "binary_data_2",  
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
  filename <- paste0("binary_data_2_test_", N, ".rds")
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
                                'binary_data_2',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- binary_data_2(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'binary:logistic')
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'binary:logistic')
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'binary:logistic', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- InvariantTargetPrediction(data$X4, as.factor(data$X3), cond_var, verbose = FALSE, alpha = 0.05) 
    local_test <- localTests('dag{
      X1 -> X3
      X2 -> X3
      X2 -> X4
    }', data = data, type="cis.pillai")[3,] 
    
    return(data.frame(
      test_number = i,
      data_generation = "binary_data_2",  
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
  filename <- paste0("binary_data_2_test_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ Categorical Data ######################

cl <- makeCluster(detectCores() - 1)

for (N in c(100, 200, 500, 1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100

  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'diff_data_types',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- diff_data_types(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'multi:softprob', num_class = 4)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'multi:softprob', num_class = 4)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2, data$X1)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG, data = data, type="cis.loess", R = 250)[2,] 
    
    
    return(data.frame(
      test_number = i,
      data_generation = "diff_data_types",  
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
  filename <- paste0("diff_data_types_test_", N, ".rds")
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
                                'diff_data_types',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    data <- diff_data_types(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'multi:softprob', num_class = 4)
      
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = X4 ~ X3 + X2, p = p, objective = 'multi:softprob', num_class = 4)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$X2)
    gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)
    local_test <- localTests(DAG, data = data, type="cis.loess")[2,] 
    kci_test <- KCI(data$X3, data$X4, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    
    return(data.frame(
      test_number = i,
      data_generation = "diff_data_types",  
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
  filename <- paste0("diff_data_types_false_", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)

########################################################################################

################ 20 Zs Data ######################

###############   DAGs  ################################
DAG_20Z <- 'dag {
  Z1 -> X
  Z1 -> Y
  Z10 -> X
  Z10 -> Y
  Z11 -> X
  Z11 -> Y
  Z12 -> X
  Z12 -> Y
  Z13 -> X
  Z13 -> Y
  Z14 -> X
  Z14 -> Y
  Z15 -> X
  Z15 -> Y
  Z16 -> X
  Z16 -> Y
  Z17 -> X
  Z17 -> Y
  Z18 -> X
  Z18 -> Y
  Z19 -> X
  Z19 -> Y
  Z2 -> X
  Z2 -> Y
  Z20 -> X
  Z20 -> Y
  Z3 -> X
  Z3 -> Y
  Z4 -> X
  Z4 -> Y
  Z5 -> X
  Z5 -> Y
  Z6 -> X
  Z6 -> Y
  Z7 -> X
  Z7 -> Y
  Z8 -> X
  Z8 -> Y
  Z9 -> X
  Z9 -> Y
}'
DAG_18Z <- 'dag {
  Z1 -> X
  Z1 -> Y
  Z10 -> X
  Z10 -> Y
  Z11 -> X
  Z11 -> Y
  Z12 -> X
  Z12 -> Y
  Z13 -> X
  Z13 -> Y
  Z14 -> X
  Z14 -> Y
  Z15 -> X
  Z15 -> Y
  Z16 -> X
  Z16 -> Y
  Z17 -> X
  Z17 -> Y
  Z18 -> X
  Z18 -> Y
  Z2 -> X
  Z2 -> Y
  Z3 -> X
  Z3 -> Y
  Z4 -> X
  Z4 -> Y
  Z5 -> X
  Z5 -> Y
  Z6 -> X
  Z6 -> Y
  Z7 -> X
  Z7 -> Y
  Z8 -> X
  Z8 -> Y
  Z9 -> X
  Z9 -> Y
}'

DAG_16Z <- 'dag {
  Z1 -> X
  Z1 -> Y
  Z10 -> X
  Z10 -> Y
  Z11 -> X
  Z11 -> Y
  Z12 -> X
  Z12 -> Y
  Z13 -> X
  Z13 -> Y
  Z14 -> X
  Z14 -> Y
  Z15 -> X
  Z15 -> Y
  Z16 -> X
  Z16 -> Y
  Z2 -> X
  Z2 -> Y
  Z3 -> X
  Z3 -> Y
  Z4 -> X
  Z4 -> Y
  Z5 -> X
  Z5 -> Y
  Z6 -> X
  Z6 -> Y
  Z7 -> X
  Z7 -> Y
  Z8 -> X
  Z8 -> Y
  Z9 -> X
  Z9 -> Y
}'


DAG_14Z <- 'dag {
  Z1 -> X
  Z1 -> Y
  Z10 -> X
  Z10 -> Y
  Z11 -> X
  Z11 -> Y
  Z12 -> X
  Z12 -> Y
  Z13 -> X
  Z13 -> Y
  Z14 -> X
  Z14 -> Y
  Z2 -> X
  Z2 -> Y
  Z3 -> X
  Z3 -> Y
  Z4 -> X
  Z4 -> Y
  Z5 -> X
  Z5 -> Y
  Z6 -> X
  Z6 -> Y
  Z7 -> X
  Z7 -> Y
  Z8 -> X
  Z8 -> Y
  Z9 -> X
  Z9 -> Y
}'

###########################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'random_Z_effects',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    
    data <- random_Z_effects(N, Zs = 20)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z11, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18, data$Z19, data$Z20)
    gcm_test <- gcm.test(data$X, data$Y, Z = cond_var)
    kci_test <- KCI(data$X, data$Y, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG_20Z, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "random_z_effects_20",  
      CI_statement = "X _||_ Y | Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13, Z14, Z15, Z16, Z17, Z18, Z19, Z20",     
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
  filename <- paste0("random_Z_effects_20", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)
########################################################################################


cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'random_Z_effects',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    
    data <- random_Z_effects(N, Zs = 20)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z11, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18)
    gcm_test <- gcm.test(data$X, data$Y, Z = cond_var)
    kci_test <- KCI(data$X, data$Y, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG_18Z, data = data, type="cis.loess", R = 250)[2,] #test with cis.loess
    
    
    return(data.frame(
      test_number = i,
      data_generation = "random_z_effects_18",  
      CI_statement = "X _||_ Y | Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13, Z14, Z15, Z16, Z17, Z18",     
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
  filename <- paste0("random_Z_effects_18", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)
########################################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(100,200,500,1000, 1500)) {
  p <- 0.85
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'random_Z_effects',
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
    seed <- as.integer(Sys.time()) + Sys.getpid()
    set.seed(seed)
    
    data <- random_Z_effects(N, Zs = 20)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16, p = p)
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test1 <- list()
    test2 <- list()
    for (j in 1:100) {
      test <- TestGenerator(data = data, formula = formula = X ~ Y + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16, p = p)
      test1[[j]] <- test[1] 
      test2[[j]] <- test[2]
    }
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
    
    cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z11, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16)
    gcm_test <- gcm.test(data$X, data$Y, Z = cond_var)
    kci_test <- KCI(data$X, data$Y, cond_var, alpha = 0.05, GP = FALSE, verbose = FALSE) 
    local_test <- localTests(DAG_16Z, data = data, type="cis.loess", R = 250)[2,]
    
    
    return(data.frame(
      test_number = i,
      data_generation = "random_z_effects_18",  
      CI_statement = "X _||_ Y | Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13, Z14, Z15, Z16",     
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
  filename <- paste0("random_Z_effects_16", N, ".rds")
  saveRDS(results, filename)
}

stopCluster(cl)
########################################################################################

