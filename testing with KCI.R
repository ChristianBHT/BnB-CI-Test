library(parallel)
library(pbapply)
library(ggplot2)
library(dHSIC)
library(CondIndTests)

data <- normal_fork(3200)
cond_var <- data.frame(data$X1)
start <- Sys.time()
kci_test <- KCI(Y = data$X2, E = data$X3, X = cond_var)
end <- Sys.time()
end - start

#################
# Simple Fork #
#################

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'), library('ipred'), library('CondIndTests'), library('dHSIC')))
for (N in c(200,400,800,1600,3200)){
  clusterExport(cl,  varlist=c('normal_fork', 'N'), envir=environment() )  
  res <- pblapply(cl=cl,1:100, function(i){
    set.seed(i)
    data <- normal_fork(N)
    
    cond_var <- data.frame(data$X1)
    kci_test_r <- KCI(Y = data$X2, E = data$X3, X = cond_var)
    
    #Testing the wrong condition
    kci_test_w <- dhsic.test(X = data$X3, Y = data$X2, alpha = 0.05, method = "gamma",
               kernel = "gaussian",  pairwise = FALSE,
               bandwidth = 1, matrix.input = FALSE)
    
    c(kci_test_r$pvalue, kci_test_r$testStatistic , kci_test_w$p.value, kci_test_w$statistic)
    
  })
  
  results <- do.call(rbind, res)
  colnames(results) <- c('pvalue_right','test_S_right','pvalue_wrong', 'test_S_wrong')
  write.csv(as.data.frame(results),paste0("C:/ChristianThorjussen/results/KCI_normal_fork_",N,".csv"), row.names = TRUE)
}
stopCluster(cl)

