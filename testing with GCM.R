library(parallel)
library(pbapply)
library(ggplot2)
library(GeneralisedCovarianceMeasure)

#----------------------------------------------------------------------------------------------------------------
#--------------------------GCM method----------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

set.seed(1984)
cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('model1', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- model1(N)
    cond_var <- data.frame(data$X1)
    gcm_test_r <- gcm.test(data$X2, data$X3, Z = cond_var)
    
    #Testing the wrong condition
    gcm_test_w <- gcm.test(data$X2, data$X3)
    c(gcm_test_r$p.value, gcm_test_r$test.statistic , gcm_test_w$p.value, gcm_test_w$test.statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/GCMtest_model1_",N,".csv"), row.names = TRUE)
}

stopCluster(cl)

set.seed(1985)
cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('model3', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- model3(N)
  
    cond_var <- data.frame(data$X1)
    gcm_test_r <- gcm.test(data$X2, data$X3, Z = cond_var)
    
    #Testing the wrong condition
    gcm_test_w <- gcm.test(data$X2, data$X3,  regr.method = "xgboost", max_depth = 6)
    c(gcm_test_r$p.value, gcm_test_r$test.statistic , gcm_test_w$p.value, gcm_test_w$test.statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/GCMtest_model3_",N,".csv"), row.names = TRUE)
}
########################################################################################

stopCluster(cl)

set.seed(1987)
cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('model6', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- model6(N)
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test_r <- gcm.test(Y = data$X4, X = data$X3, Z = cond_var)
    
    #Testing the wrong condition
    cond_var <- data.frame(data$X2)
    gcm_test_w <- gcm.test(Y = data$X4, X = data$X3, Z = cond_var)
    c(gcm_test_r$p.value, gcm_test_r$test.statistic , gcm_test_w$p.value, gcm_test_w$test.statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/GCMtest_model6_",N,".csv"), row.names = TRUE)
}

stopCluster(cl)
####################################################################################
set.seed(1988)
cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('model8', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(i)
    data <- model8(N)
    # Condition 1
    cond_var <- data.frame(data$X3, data$X4)
    gcm_test1_r <- gcm.test(data$X5, data$X1, Z = cond_var)
    # Condition 2

    cond_var <- data.frame(data$X1, data$X2)
    gcm_test2_r <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    #Testing the wrong conditions
    # Condition 1
    cond_var <- data.frame(data$X3)
    gcm_test1_w <- gcm.test(data$X5, data$X1, Z = cond_var)
    # Condition 2
    cond_var <- data.frame(data$X1)
    gcm_test2_w <- gcm.test(data$X4, data$X3, Z = cond_var)
    
    c(gcm_test1_r$p.value, gcm_test1_w$p.value, gcm_test2_r$p.value, gcm_test2_w$p.value)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue1_right','pvalue1_wrong','pvalue2_right','pvalue2_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/GCMtest_model8_",N,".csv"), row.names = TRUE)
}

stopCluster(cl)

####################################################################################
cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('GeneralisedCovarianceMeasure')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('model10', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:1000, function(i){
    set.seed(1990 + i)
    
    data <- model10(N)
    data$X3 <- data$X3 - 1
    # Condition 1
    cond_var <- data.frame(data$X3, data$X4)
    gcm_test1_r <- tryCatch({
      gcm.test(data$X5, data$X1, Z = cond_var, regr.method = "xgboost")
    }, error = function(e) {
      NA
    })
    
    # Condition 2
    cond_var <- data.frame(data$X1, data$X2)
    gcm_test2_r <- tryCatch({
      gcm.test(data$X4, data$X3, Z = cond_var, regr.method = "xgboost")
    }, error = function(e) {
      NA
    }) 
    
    #Testing the wrong conditions
    # Condition 1
    cond_var <- data.frame(data$X3)
    gcm_test1_w <- tryCatch({
      gcm.test(data$X5, data$X1, Z = cond_var, regr.method = "xgboost")
    }, error = function(e) {
      NA
    })
    
    # Condition 2
    cond_var <- data.frame(data$X1)
    gcm_test2_w <- tryCatch({
      gcm.test(data$X4, data$X3, Z = cond_var, regr.method = "xgboost")
    }, error = function(e) {
      NA
    }) 
    
    
    results <- list()
    # It statements to chech if output of GCM test is a list, if not i.e. NA, 
     if (class(gcm_test1_r) == 'list') {
       results$pvalue1_right <- gcm_test1_r$p.value
     } else {
       results$pvalue1_right <- NULL
     } 
     
     if (class(gcm_test1_w) == 'list') {
       results$pvalue1_wrong <- gcm_test1_w$p.value
     } else {
       results$pvalue1_wrong <- NULL
     } 
     
     if (class(gcm_test2_r) == 'list') {
       results$pvalue2_right <- gcm_test2_r$p.value
       
     } else {
       results$pvalue2_right <- NULL
     } 
     
     if (class(gcm_test2_w) == 'list') {
       results$pvalue2_wrong <- gcm_test2_w$p.value
       
     } else {
       results$pvalue2_wrong <- NULL
     } 
     
    
     return(results)
  })
  
  results <- do.call(rbind, res)
  results <- data.frame(cbind(unlist(results[,1]),unlist(results[,2]),unlist(results[,3]),unlist(results[,4])))
  colnames(results) <- c('pvalue1_right','pvalue1_wrong','pvalue2_right','pvalue2_wrong')
  # colnames(results) <- c('pvalue1_right','pvalue1_wrong','pvalue2_right','pvalue2_wrong')
  write.csv(results,paste0("C:/ChristianThorjussen/GCMtest_model10_",N,".csv"), row.names = TRUE)
}

stopCluster(cl)


