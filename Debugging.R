# Bugs and testing script.
# Testing example

data <- normal_data(400)
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

data <- normal_data(900)

output1 <- list()
output2 <- list()
R = 1000 
for (i in 1:R) {
  output1[[i]] <- NullGenerator(data = data, formula = X4 ~ X3 + X1 + X2, p = p)
  output2[[i]] <- TestGenerator(data = data, formula = X4 ~ X3 + X1 + X2, p = p)
  # Calculate progress 
  cat(sprintf("Sample: %d\r", i))
  flush.console()
  Sys.sleep(0.1)
}

output_df <- do.call(rbind, output1)
NullDist <- data.frame(output_df)
hist(as.numeric(NullDist$X1), breaks = 40)

output_df <- do.call(rbind, output2)
TestDist <- data.frame(output_df)
hist(as.numeric(TestDist$X1), breaks = 40)

test <- TestGenerator(data = data, formula = X4 ~ X3 + X1 + X2, p = p)
test[[1]]
p_value <- sum(NullDist[1] >= test[[1]]) / nrow(NullDist)
p_value

p_value <- numeric(300)
for (i in 1:300) {
  test <- TestGenerator(data = data, formula = X4 ~ X3 + X1 + X2, p = p)
  p_value[i] <- sum(NullDist[1] < test[[1]]) / nrow(NullDist)
}

hist(p_value)
