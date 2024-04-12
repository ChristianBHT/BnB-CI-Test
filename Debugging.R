# Bugs and testing script.
# Testing example

data <- normal_fork(1000)
test <- CItest_xgboost(data = data, indices = NULL, formula = X4 ~ X3 +  X1 + X2, p = p)
test

output <- list()

R = 100 # 100 cross validation
for (i in 1:R) {
  output[[i]] <- CItest_xgboost(data = data, formula = X4 ~ X3 + X1 + X2, indices = NULL, p = p)
  # Calculate progress 
  cat(sprintf("Cross-Validation sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}

output_df <- do.call(rbind, output)
output_df <- data.frame(output_df)
hist(as.numeric(output_df$X1))
hist(as.numeric(output_df$Difference.R.squared))

