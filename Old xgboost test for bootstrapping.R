CItest_xgboost <- function(formula = NULL,
                           data = NULL,
                           p = NULL,
                           objective = "reg:squarederror",
                           nrounds = 100,
                           eta = 0.1,
                           max_depth = 6,
                           num_class = NULL,
                           nthread = 1) {
  if (is.null(data)) {
    stop("Please provide some data")
  }
  
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  
  if (!(class(formula) %in% "formula")) {
    formula <- as.formula(formula)
  }
  
  # Split the data
  inTraining <- sample(1:nrow(data), size = floor(p * nrow(data)))
  training <- data[inTraining, ]
  testing <- data[-inTraining, ]
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  
  if (any(sapply(training, is.factor))) {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_features <- model.matrix(~ . - 1, data = train_features)
    test_features <- model.matrix(~ . - 1, data = test_features)
    
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  } else {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  }
  
  if (objective %in% "multi:softmax") {
    params <- list(
      eta = eta,
      max_depth = max_depth,
      num_class = num_class
    )
  } else {
    params <- list(
      eta = eta,
      max_depth = max_depth
    )
  }
  
  # Train the XGBoost models
  m_1 <- xgboost::xgb.train(data = train_matrix,
                            params = params,
                            nrounds = nrounds,
                            verbose = 0,
                            nthread = nthread)
  
  if (objective %in% c("binary:logistic", "multi:softmax")) {
    # Predict on test set using model 1
    predictions <- predict(m_1, test_matrix)
    # Predictions of binary outcome are probabilities
    if (objective %in% "binary:logistic") {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 1
    predictions <- predict(m_1, newdata = test_matrix)
    # Calculate RMSE
    mod1_metric1 <- Metrics::rmse(test_label, predictions)
    # Calculate R2
    mod1_metric2 <- cor(predictions, test_label)^2
  }
  
  training[independent[[1]]] <- sample(training[[independent[1]]]) 
  testing[independent[[1]]] <- sample(testing[[independent[1]]]) #Permutation in both sets 
  # Creating new feature set, same steps as above
  if (any(sapply(training, is.factor))) {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_features <- model.matrix(~ . - 1, data = train_features)
    test_features <- model.matrix(~ . - 1, data = test_features)
    
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  } else {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  }
  
  m_2 <- xgboost::xgb.train(data = train_matrix,
                            objective = objective,
                            params = params,
                            nrounds = nrounds,
                            verbose=0,
                            nthread = nthread)
  
  if (objective %in% c("binary:logistic", "multi:softmax")) {
    predictions <- predict(m_2, test_matrix)
    if (objective %in% "binary:logistic") {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    mod2_metric1 <- conf_matrix$overall[1]
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(m_2, newdata = test_matrix)
    mod2_metric1 <- Metrics::rmse(test_label, predictions)
    mod2_metric2 <- cor(predictions, test_label)^2
  }
  
  if (objective %in% c("binary:logistic", "multi:softmax")) {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    
  } else {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    
  }
  return(result)
}
