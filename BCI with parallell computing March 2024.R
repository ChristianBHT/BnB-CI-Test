rm(list = ls())
library(DirichletReg)
library(ipred)
library(caret)
library(Metrics)
library(parallel)
library(pbapply)

############################ Sim non_lin_fork ############################ 
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  for (N in c(200, 400)){
    p = 1 - (100/N)
    R <- 1000
    seed <- N
    clusterExport(cl,  varlist=c('non_lin_fork', 'xgboost_test', 'N', 'R', 'p', 'seed'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      seed <- seed + 1
      set.seed(seed)
      data <- non_lin_fork(N)
      output <- list()
      
      for (i in 1:R) {
        
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2 + X1, 
                                    p = p, 
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE, 
                                    weights = NULL)
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
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/results/BCI_non_lin_fork_true_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
 


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
 
  
  ##################simulate_Z_effects_advanced#########################
  