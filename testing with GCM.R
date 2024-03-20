library(parallel)
library(pbapply)
library(ggplot2)
library(GeneralisedCovarianceMeasure)

#----------------------------------------------------------------------------------------------------------------
#--------------------------GCM method----------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('normal_fork', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- normal_fork(N)
    cond_var <- data.frame(data$X1)
    gcm_test_r <- gcm.test(data$X2, data$X3, Z = cond_var)
    
    #Testing the wrong condition
    gcm_test_w <- gcm.test(data$X2, data$X3)
    c(gcm_test_r$p.value, gcm_test_r$test.statistic , gcm_test_w$p.value, gcm_test_w$test.statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/results/GCM_normal_fork_",N,".csv"), row.names = TRUE)
}
stopCluster(cl)

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('non_lin_fork', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- non_lin_fork(N)
    cond_var <- data.frame(data$X1)
    gcm_test_r <- gcm.test(data$X2, data$X3, Z = cond_var)
    
    #Testing the wrong condition
    gcm_test_w <- gcm.test(data$X2, data$X3)
    c(gcm_test_r$p.value, gcm_test_r$test.statistic , gcm_test_w$p.value, gcm_test_w$test.statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/results/GCM_non_lin_fork_",N,".csv"), row.names = TRUE)
}

