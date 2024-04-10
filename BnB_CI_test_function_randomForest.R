library(randomForest)
data <- poisson_adjusted(1000)
formula <- X4 ~ X3 + X2 + X1
p = 0.8
randomForest_test <- function(data = NULL, formula = NULL, p = NULL,
                              ntree = 500, 
                              n_folds = 10, bootstrap_sample = FALSE,
                              ...) {
  
  if (is.null(data)) {
    stop("Please provide some data")
  }
  
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  
  if (!(class(formula) %in% "formula")) {
    formula <- as.formula(formula)
  }
  
  # Prepare the data
  if (bootstrap_sample) {
    resample <- data[sample.int(nrow(data), replace = TRUE), ]
  } else {
    resample <- data
  }
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]] 
  
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]
  
  rf_model_1 <- randomForest(formula, data = training, ntree = ntree)
  
  predictions <- predict(rf_model_1, newdata = testing)
  
  
  if (any(grepl("regression", rf_model_1$type))) {
    mod_metric1 <- with(testing, mean((predictions - testing[[dependent]])^2))  # MSE
    mod_metric2 <- cor(predictions, testing[[dependent]])^2  # R^2
  } else {
    confusionMat <- table(predictions, testing[[dependent]])
    mod_metric1 <- sum(diag(confusionMat)) / sum(confusionMat)  # Accuracy
    mod_metric2 <- NA 
  }
  
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]])
  testing[independent[[1]]] <- sample(testing[[independent[1]]]) 
  
  rf_model_2 <- randomForest(formula, data = training, ntree = ntree)
  
  
  result <- list(mod_metric1 = mod_metric1, mod_metric2 = mod_metric2)
  
  return(result)
}