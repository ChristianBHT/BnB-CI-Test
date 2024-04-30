library(dplyr)
library(parallel)
library(pbapply) 
library(dagitty)
library(xgboost)
library(caret)
library(Metrics)
library(DescTools)

library(nortest)
library(lawstat)
library(DescTools)
library(moments)
library(nnet)  
library(e1071)
library(DescTools)

########### First we need to check if Type I error is controlled ################

try(stopCluster(cl))

cl <- makeCluster(detectCores() - 1)

for (N in c(200, 500, 800, 1500, 2500, 4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 200
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'CategorizeInteractiondData',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    data <- CategorizeInteractiondData(N)
    output <- list()
    
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'multi:softprob', num_class = 4)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = p, objective = 'multi:softprob', num_class = 4)
    test1 <- test[1] 
    test2 <- test[2]
     
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation 1",  
      CI_statement = "X4 _||_ X3 | X2, X1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("simulation_1_type_1_", N,".rds")
  saveRDS(results, filename)
}
stopCluster(cl)

################################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(200, 500, 800, 1500, 2500, 4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'multinominal',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    
    data <- multinominal(N, zeta = 4)
    data$X <- as.factor(data$X)
    data$Y <- as.integer(as.factor(data$Y))-1
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))

    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2 + Z1 , p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 4",  
      CI_statement = "X _||_ Y | Z2, Z1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_4_type_1_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(200, 500, 800, 1500, 2500, 4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'multinominal',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    
    data <- multinominal(N, zeta = 2)
    data$X <- as.factor(data$X)
    data$Y <- as.integer(as.factor(data$Y))-1
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2 + Z1 , p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    

    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 2",  
      CI_statement = "X _||_ Y | Z2, Z1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_2_type_1_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(200, 500, 800, 1500, 2500, 4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator', 
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'multinominal',
                                'N', 
                                'R', 
                                'p', 
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    
    
    data <- multinominal(N, zeta = 1)
    data$X <- as.factor(data$X)
    data$Y <- as.integer(as.factor(data$Y))-1
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2 + Z1 , p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 1",  
      CI_statement = "X _||_ Y | Z2, Z1",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_1_type_1_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################




