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
    clusterExport(cl,  varlist=c('non_lin_fork', 'xgboost_test', 'N', 'R', 'p'), envir=environment())
    clusterEvalQ(cl, c(library('DirichletReg'),
                       library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost')
    ))
    
    res <- pblapply(cl=cl,1:100, function(i){
      
      data <- non_lin_fork(N)
      output <- list()
      
      for (i in 1:R) {
        
        
        output[[i]] <- xgboost_test(data = data, 
                                    formula = X3 ~ X2, 
                                    p = p, 
                                    eta = 0.1, 
                                    bootstrap_sample = TRUE)
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
    
    write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/results/non_lin_fork_false_",N,"_xgbootest.csv"), row.names = TRUE)
  }
  stopCluster(cl)
 
  
  ##################simulate_Z_effects_advanced#########################
  # data <- simulate_Z_effects_advanced(1000)
  # test <- xgboost_test(data = data, 
  #                      formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20,
  #                      p = p, 
  #                      eta = 0.1, 
  #                      bootstrap_sample = TRUE, 
  #                      weights = NULL)
  # test
  # 
  # output <- list()
  # R = 1000
  # for (i in 1:R) {
  #   
  #   output[[i]] <- xgboost_test(data = data, formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20 , p = p, eta = 0.1,  bootstrap_sample = TRUE, weights = NULL)
  #   # Calculate progress 
  #   cat(sprintf("Bootstrap sample: %d\r", i))
  #   flush.console()
  #   Sys.sleep(0.1)
  #   
  # }
  # output_df <- data.frame(output_df)
  # hist(as.numeric(output_df$diff_met2), breaks = 30)
  # 
  # cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z11, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18, data$Z19, data$Z20)
  # gcm_test_r <- gcm.test(data$Y, data$X, Z = cond_var)
  # gcm_test_r
  
  cl <- makeCluster(detectCores()-1, type = "PSOCK")
  
  
  for (N in c(100)){
    p = 0.8
    R <- 10 # Number of bootstrap samples
    no_tests <- 25 # Number of tests
    seed <- N # Set seed for reproducibility
    
    output_matrix <- matrix(vector("list", no_tests * 2), nrow=no_tests, ncol = 2)
    
    clusterExport(cl,  varlist=c('random_Z_effects', 'xgboost_test', 'N', 'R', 'p', 'seed', 'no_tests'), envir=environment())
    clusterEvalQ(cl, c(library('ipred'),
                       library('caret'),
                       library('Metrics'),
                       library('dplyr'),
                       library('xgboost'),
                       library('GeneralisedCovarianceMeasure')))
    
    results <- pblapply(cl=cl, 1:no_tests, function(i){
      local_seed <- seed + i # Update seed for each test
      set.seed(local_seed)
      
      data <- random_Z_effects(N)
      output <- list()
      
      for (j in 1:R) {
        output[[j]] <- xgboost_test(data = data, 
                                    formula = Y ~ X + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11 + Z12 + Z13 + Z14 + Z15 + Z16 + Z17 + Z18 + Z19 + Z20 , 
                                    p = p, 
                                    eta = 0.1,  
                                    bootstrap_sample = TRUE, 
                                    weights = NULL)
      }
      
      cond_var <- data.frame(data$Z1, data$Z2, data$Z3, data$Z4, data$Z5, data$Z6, data$Z7, data$Z8, data$Z9, data$Z10, data$Z11, data$Z12, data$Z13, data$Z14, data$Z15, data$Z16, data$Z17, data$Z18, data$Z19, data$Z20)
      gcm_test <- gcm.test(data$Y, data$X, Z = cond_var)
      
      return(list(Boot_CI = output, gcm_test_p = gcm_test$p.value, gcm_reject = gcm_test$reject))
    })
    
  
    # Save the output_matrix to a file
    filename <- paste0("output_matrix_N_", N, ".rds")
    saveRDS(results, filename)
  }
  
  stopCluster(cl)
  
  
output_matrix_N_100 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/output_matrix_N_100.rds")
diff_met1_values <- sapply(output_matrix_N_3000[[1]]$output, function(x) x$diff_met1)  
diff_met2_values <- sapply(output_matrix_N_3000[[1]]$output, function(x) x$diff_met2)  
hist(diff_met2_values)
  
  