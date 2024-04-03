randomForest_test <- function(data = NULL, formula = NULL, p = NULL,
                              ntree = 500, mtry = NULL, 
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
  
  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  trainingData <- resample[inTraining, ]
  testData <- resample[-inTraining, ]
  
  # Fit the randomForest model
  rf_model <- randomForest(formula, data = trainingData, ntree = ntree, mtry = mtry, ...)
  
  # Predictions and Performance
  predictions <- predict(rf_model, newdata = testData)
  
  # Performance metrics
  # For regression:
  if (any(grepl("reg", rf_model$call$objective))) {
    mod_metric1 <- with(testData, mean((predictions - actual)^2))  # MSE
    mod_metric2 <- cor(predictions, testData[[dependent]])^2  # R^2
  } else {
    # For classification:
    confusionMat <- table(predictions, testData[[dependent]])
    mod_metric1 <- sum(diag(confusionMat)) / sum(confusionMat)  # Accuracy
    mod_metric2 <- NA  # Another relevant metric for classification
  }
  
  # You might want to perform the resampling logic here for another model (e.g., with shuffled labels)
  
  # Return results
  result <- list(mod_metric1 = mod_metric1, mod_metric2 = mod_metric2)
  
  return(result)
}