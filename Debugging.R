# Bugs and testing script.
# Testing example
data <- normal_data(1000)
output <- list()
for (i in 1:1000) {
  output[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
   
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  
}

NullDist <- data.frame(do.call(rbind, output))

test1 <- list()
test2 <- list()
for (i in 1:100) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X2 + X1, p = 0.85)
  test1[i] <- test[1] 
  test2[i] <- test[2]
  
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}

p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = unlist(test1), test2_metric = unlist(test2))
cond_var <- data.frame(data$X2, data$X1)
gcm_test <- gcm.test(data$X3, data$X4, Z = cond_var)


res <- data.frame(
  data_generation = "normal_data",  
  CI_statement = "X4 _||_ X3 |  X2, X1",     
  pvalue1 = p_value1,
  p_value2 = p_value2,
  param_p_value1 = param_p_value1,
  param_p_value2 = param_p_value2,
  GCM_pvalue = gcm_test$p.value  
)
