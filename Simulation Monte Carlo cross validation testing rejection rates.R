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

################### Rejecting Null ############################

cl <- makeCluster(detectCores() - 1)

for (N in c(4000)) {
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
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 3",  
      CI_statement = "X _||_ Y | Z2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_4_rejecting_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 200
  
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
    
    
    data <- multinominal(N, zeta = 3)
    data$X <- as.factor(data$X)
    data$Y <- as.integer(as.factor(data$Y))-1
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2 , p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 3",  
      CI_statement = "X _||_ Y | Z2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_3_rejecting_", N,".rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(4000)) {
  p <- 0.80
  R <- 1000
  no_tests <- 200
  
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
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 2",  
      CI_statement = "X _||_ Y | Z2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_2_rejecting_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

# Using the condition of a uniform distrbution under null

cl <- makeCluster(detectCores() - 1)

for (N in c(800)) {
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
    
    
    data <- multinominal(N, zeta = 3)
    data$X <- as.factor(data$X)
    data$Y <- as.integer(as.factor(data$Y))-1
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    output <- list()
    for (j in 1:R) {
      test[[j]] <- TestGenerator(data = data, formula = Y ~ X + Z2 , p = p, objective = 'multi:softprob', num_class = 3)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    
    
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 3",  
      CI_statement = "X _||_ Y | Z2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_3_rejecting_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

cl <- makeCluster(detectCores() - 1)

for (N in c(200, 500, 800, 1500, 2500)) {
  p <- 0.80
  R <- 1000
  no_tests <- 200
  
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
      output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
      # cat(sprintf("Sample: %d\r", j))
      # flush.console()
    }
    
    NullDist <- data.frame(do.call(rbind, output))
    hist(NullDist$Metric1)
    hist(NullDist$Metric2)
    
    test <- TestGenerator(data = data, formula = Y ~ X + Z2, p = p, objective = 'multi:softprob', num_class = 3)
    test1 <- test[1]
    test2 <- test[2]
    
    p_values <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
    
    
    return(data.frame(
      test_number = i,
      data_generation = "Simulation multinominal zeta = 2",  
      CI_statement = "X _||_ Y | Z2",     
      pvalue1 = p_values[1],
      pvalue2 = p_values[2],
      p_mean =  p_values[3]
    ))
  })
  
  # Save output
  filename <- paste0("multinominal_1_rejecting_", N,"_new.rds")
  saveRDS(results, filename)
}

stopCluster(cl)
###########################################################################

##################### Testing difference in distributions #################


cl <- makeCluster(detectCores() - 1)

p <- 0.80
R <- 1500
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
  
  
  data <- multinominal(500, zeta = 2)
  data$X <- as.factor(data$X)
  data$Y <- as.integer(as.factor(data$Y))-1
  
  output <- list()
  for (j in 1:R) {
    output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = 0.8, objective = 'multi:softprob', num_class = 3)
  }
  
  NullDist <- data.frame(do.call(rbind, output))
  
  test <- list()
  for (j in 1:R) {
    test[[j]] <- TestGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = 0.8, objective = 'multi:softprob', num_class = 3)
  }
  TestDist <- data.frame(do.call(rbind, test))
  
  # plot(ecdf(NullDist$Metric2), xlim = range(c(NullDist$Metric2, TestDist$Metric2)))
  # plot(ecdf(TestDist$Metric2), add = TRUE, lty = "dashed")
  
  t_test1 <- t.test(NullDist$Metric1, TestDist$Metric1, alternative = "g")
  wilcox_test1 <- wilcox.test(NullDist$Metric1, TestDist$Metric1, alternative = "g")
  ks_test1 <- ks.test(NullDist$Metric1, TestDist$Metric1, alternative = "l")  
  
  t_test2 <- t.test(NullDist$Metric2, TestDist$Metric2, alternative = "l")
  wilcox_test2 <- wilcox.test(NullDist$Metric2, TestDist$Metric2, alternative = "l")
  ks_test2 <- ks.test(NullDist$Metric2, TestDist$Metric2, alternative = "g")
  
  return(data.frame(
    test_number = i,
    data_generation = "Simulation multinominal zeta = 2",  
    CI_statement = "X _||_ Y | Z2, Z1",     
    ttest1 = t_test1$p.value,
    wilcox1 = wilcox_test1$p.value,
    ks1 =  ks_test1$p.value,
    ttest2 = t_test2$p.value,
    wilcox2 = wilcox_test2$p.value,
    ks2 =  ks_test2$p.value
    
  ))
})

# Save output
filename <- paste0("multi_2_low_power.rds")
saveRDS(results, filename)
