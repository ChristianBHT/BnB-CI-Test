rm(list = ls())
library(DirichletReg)
library(ipred)
library(caret)
library(Metrics)
library(parallel)
library(pbapply)

############################ Sim 1 ############################ 
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400)){
    p = 0.8
    R <- 1000
    clusterExport(cl,  varlist=c('model1', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model1(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2 + X1, 
                                    p = p, 
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model1_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
 

  set.seed(1984)
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1 - (100/N) 
    R <- 1000
    clusterExport(cl,  varlist=c('model1', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model1(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2, 
                                    p = p, 
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model1_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
 
  ############################ Sim 2 ############################

  set.seed(1985)
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model3', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model3(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2 + X1, 
                                    p = p,
                                    max_depth = c(2,3,4),
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model3_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  set.seed(1984)
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1 - (100/N) 
    R <- 1000
    clusterExport(cl,  varlist=c('model3', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model3(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2, 
                                    p = p,
                                    max_depth = c(2,3,4),
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model3_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  ############################ Sim 3 ############################
  set.seed(1986)
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model6', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model6(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X4 ~ X3 + X2 + X1, 
                                    p = p,
                                    max_depth = c(2,3,4,5),
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model6_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  set.seed(1986)
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1 - (100/N) 
    R <- 1000
    clusterExport(cl,  varlist=c('model6', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model6(N)
      output <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X4 ~ X3 + X2, 
                                    p = p,
                                    max_depth = c(2,3,4,5),
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = weights)
      }
      
      output_df <- do.call(rbind, output)
      output_df <- data.frame(output_df)
      
      c(mean(unlist(output_df$diff_met1), na.rm = TRUE),
        sd(unlist(output_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$diff_met2), na.rm = TRUE),
        sd(unlist(output_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output_df$mod2_metric2), na.rm = TRUE))
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model6_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  ############################ Sim 4 ############################
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model8', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model8(N)
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X5 ~ X1 + X3 + X4, 
                                     p = p,
                                     max_depth = c(3,4,5),
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1 + X2, 
                                     p = p,
                                     max_depth = c(3,4,5),
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
      }
      
      
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model8_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model8', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model8(N)
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X5 ~ X1 + X3 , 
                                     p = p,
                                     max_depth = c(3,4,5),
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1 , 
                                     p = p,
                                     max_depth = c(3,4,5),
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
      }
      
      
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model8_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  ############################ Sim 5 ############################
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)

      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                              formula = X5 ~ X1 + X3 + X4, 
                              p = p,
                              objective =  "binary:logistic",
                              max_depth = c(1,2,3),
                              nrounds = 50,
                              eta = 0.1,
                              subsample = 0.8,
                              bootstrap_sample = TRUE, 
                              weights = weights)
        
        data$X3 <- as.numeric(data$X3)
        data$X3 <- data$X3 - 1  
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1 + X2, 
                                     objective = "multi:softmax",
                                     num_class = 3,
                                     p = p,
                                     max_depth = c(1,2,3),
                                     subsample = 0.8, 
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
      
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
 
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X5 ~ X1 + X3, 
                                     p = p,
                                     objective =  "binary:logistic",
                                     max_depth = c(1,2,3),
                                     nrounds = 50,
                                     eta = 0.1,
                                     subsample = 0.8,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        data$X3 <- as.numeric(data$X3)
        data$X3 <- data$X3 - 1  
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1, 
                                     objective = "multi:softmax",
                                     num_class = 3,
                                     p = p,
                                     max_depth = c(1,2,3),
                                     subsample = 0.8, 
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)

  
  
  ############################ Sim 5 continous outcome ############################
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X1 ~ X5 + X3 + X4, 
                                     p = p,
                                     max_depth = c(1,2,3,4),
                                     nrounds = 50,
                                     eta = 0.1,
                                     subsample = 0.8,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X4 ~ X3 + X1 + X2, 
                                     p = p,
                                     max_depth = c(1,2,3,4),
                                     subsample = 0.8, 
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_true_continous_outcome_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X1 ~ X5 + X3, 
                                     p = p,
                                     max_depth = c(2,3,4,5,6),
                                     nrounds = 50,
                                     eta = 0.1,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X4 ~ X3 + X1, 
                                     p = p,
                                     max_depth = c(2,3,4,5,6),
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_false_continous_outcome_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  
  
  
  
  
  
  
  
  ############################ Sim 5 continous outcome v2############################
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 2
    clusterExport(cl,  varlist=c('model10_v2', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10_v2(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X5 ~ X1 + X3 + X4, 
                                     p = p,
                                     objective =  "binary:logistic",
                                     max_depth = c(1,2,3),
                                     nrounds = 50,
                                     eta = 0.1,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        data$X3 <- as.numeric(data$X3)
        data$X3 <- data$X3 - 1  
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1 + X2, 
                                     objective = "multi:softmax",
                                     num_class = 3,
                                     p = p,
                                     max_depth = c(1,2,3),
                                     subsample = 0.8, 
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X5 ~ X1 + X3, 
                                     p = p,
                                     objective =  "binary:logistic",
                                     max_depth = c(1,2,3),
                                     nrounds = 50,
                                     eta = 0.1,
                                     subsample = 0.8,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        data$X3 <- as.numeric(data$X3)
        data$X3 <- data$X3 - 1  
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X3 ~ X4 + X1, 
                                     objective = "multi:softmax",
                                     num_class = 3,
                                     p = p,
                                     max_depth = c(1,2,3),
                                     subsample = 0.8, 
                                     nrounds = 50,
                                     eta = 0.1, 
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  
  ############################ Sim 5 continous outcome ############################
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400, 800, 1600, 3200)){
    p = 1- (100/N)
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        data$X3 <- as.factor(data$X3)
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X1 ~ X5 + X3 + X4, 
                                     p = p,
                                     max_depth = 6,
                                     nrounds = 100,
                                     eta = 0.1,
                                     early_stopping = 100,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        output2[[i]] <- xgboost_test(data = data, 
                                     formula = X4 ~ X3 + X1 + X2, 
                                     p = p,
                                     max_depth = 6,
                                     nrounds = 100,
                                     eta = 0.1, 
                                     early_stopping = 100,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_true_continous_outcome_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(3200)){
    p = 0.8
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'test_function', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X1 ~ X5 + X3, 
                                     p = p,
                                     max_depth = c(5,6),
                                     nrounds = 100,
                                     early_stopping = 30, 
                                     eta = 0.1,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
        output2[[i]] <- test_function(data = data, 
                                      formula = X1 ~ X5 + X3, 
                                      p = p,
                                      bootstrap_sample = TRUE, 
                                      weights = weights)
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      output2_df <- do.call(rbind, output2)
      output2_df <- data.frame(output2_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE), 
        mean(unlist(output2_df$diff_met1), na.rm = TRUE),
        sd(unlist(output2_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$diff_met2), na.rm = TRUE),
        sd(unlist(output2_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output2_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output2_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output2_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output2_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2',
                           'mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_false_continous_outcome_",N,"_xgbootest_vs_bagging.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200,400,800)){
    p = 0.8
    R <- 1000
    clusterExport(cl,  varlist=c('model10', 'xgboost_test', 'test_function', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- model10(N)
      
      output1 <- list()
      output2 <- list()
      
      for (i in 1:R) {
        
        weights <- matrix(rdirichlet(1, c(rep(1,nrow(data)))), nrow = nrow(data), ncol = 1)
        
        output1[[i]] <- xgboost_test(data = data, 
                                     formula = X4 ~ X3 + X1, 
                                     p = p,
                                     max_depth = c(5,6),
                                     nrounds = 100,
                                     early_stopping = 30, 
                                     eta = 0.1,
                                     bootstrap_sample = TRUE, 
                                     weights = weights)
        
      
        
      }
      
      output1_df <- do.call(rbind, output1)
      output1_df <- data.frame(output1_df)
      
      c(mean(unlist(output1_df$diff_met1), na.rm = TRUE),
        sd(unlist(output1_df$diff_met1), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met1), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$diff_met2), na.rm = TRUE),
        sd(unlist(output1_df$diff_met2), na.rm = TRUE),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.025),
        quantile(unlist(output1_df$diff_met2), na.rm = TRUE, probs = 0.975),
        mean(unlist(output1_df$mod1_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric1), na.rm = TRUE),
        mean(unlist(output1_df$mod1_metric2), na.rm = TRUE),
        mean(unlist(output1_df$mod2_metric2), na.rm = TRUE))
      
      
    })
    
    results <- do.call(rbind, res)
    colnames(results) <- c('mean_diff_1','sd_mean_1', 'diff1_lower', 'diff1_upper',
                           'mean_diff_2','sd_mean_2', 'diff2_lower', 'diff2_upper',
                           'mean_mod1_metric1','mean_mod2_metric1','mean_mod1_metric2','mean_mod2_metric2') 
    
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/model10_false2_continous_outcome_",N,"_xgbootest_vs_bagging.csv"), row.names = TRUE)
  }
  stopCluster(cl)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  