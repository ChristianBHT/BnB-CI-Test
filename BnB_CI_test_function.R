library(xgboost)
library(DirichletReg)
library(ipred)
library(caret)
library(Metrics)
library(dplyr)

xgboost_test <- function(data = NULL, 
                         formula = NULL,  
                         p = NULL,  
                         objective = "reg:squarederror", 
                         early_stopping = 10, 
                         nrounds = 150, 
                         eta = 0.1, 
                         max_depth = c(2,3,4), 
                         num_class = NULL, 
                         subsample = 0.8, 
                         n_folds = 5, 
                         alpha = 0,
                         lambda = 0,
                         bootstrap_sample = FALSE, 
                         weights = NULL) {
  
  if (is.null(data)) {
    stop("Please provide some data")
  }
  
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  
  if (!is.null(weights)) {
    # Check length 
    if (length(weights) != nrow(data)) {
      stop("Error: 'weights' should have the same number of entries as the rows in 'data'")
    }
  }
  
  if (!(class(formula) %in% "formula")) {
    formula <- as.formula(formula)
  }
  
  if (bootstrap_sample) {
    if (!is.null(weights)) {
      index <- sample.int(nrow(data), replace = TRUE, prob = weights)
      resample <- data[index, ]
    } else {
      index <- sample.int(nrow(data), replace = TRUE)
      resample <- data[index, ]
    }
  } else {
    # Do not resample   
    resample <- data
  }
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]] 
  
  if (any(sapply(resample, is.factor))) {
    
    features <- resample[independent]
    label <- resample[[dependent]]
    features <- model.matrix(~ . - 1, data = features)
    
    data_matrix <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
    
  } else {
     
    features <- resample[independent]
    label <- resample[[dependent]]
    
    data_matrix <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
    
  }
  
  
  nrounds_values <- nrounds
  
  best_max_depth <- NULL
  best_nrounds <- NULL
  best_test <- Inf
  best_iteration <- NULL
  
  # Perform grid search over hyperparameters
  for (depth in max_depth) {
    params <- list(
      eta = eta,
      max_depth = depth
    )
    
    for (fold in 1:n_folds) {
      fold_size <- floor(nrow(data_matrix) / n_folds)
      test_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
      train_indices <- setdiff(1:nrow(data_matrix), test_indices)
      
      train_data <- data_matrix[train_indices, ]
      test_data <- data_matrix[test_indices, ]
      
      watchlist <- list(train = train_data, test = test_data)
      if (objective %in% "multi:softmax") {
        model <- xgb.train(
          data = watchlist$train,
          objective = objective,
          params = params,
          nrounds = nrounds,
          num_class = num_class,  
          early_stopping_rounds = early_stopping,
          subsample = subsample,
          nthread = 1,
          watchlist = watchlist,
          verbose = FALSE
        )
      } else {
        model <- xgb.train(
          data = watchlist$train,
          objective = objective,
          params = params,
          nrounds = nrounds,
          early_stopping_rounds = early_stopping,
          subsample = subsample,
          nthread = 1,
          watchlist = watchlist,
          verbose = FALSE
        )
      }
      
      
      if (objective %in% c("binary:logistic")) {
        best_iteration <-  which.min(model$evaluation_log$test_logloss)
        min_cv <- model$evaluation_log$test_logloss[best_iteration]
      } else if (objective %in% "multi:softmax") {
        best_iteration <-  which.min(model$evaluation_log$test_mlogloss)
        min_cv <- model$evaluation_log$test_mlogloss[best_iteration]
      } else {
        best_iteration <-  which.min(model$evaluation_log$test_rmse)
        min_cv <- model$evaluation_log$test_rmse[best_iteration]
      }
      
      
      if (min_cv < best_test) {
        best_test <- min_cv
        best_max_depth <- depth
        best_nrounds <- best_iteration
      }
    }
  }
  
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]
  
  
  if (any(sapply(training, is.factor))) {
    
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_features <- model.matrix(~ . - 1, data = train_features)
    test_features <- model.matrix(~ . - 1, data = test_features)
    
    
    train_matrix <- xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
    
    
  } else {
     
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    
    train_matrix <- xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
    
  }
  
  # Model1
 
  if (objective %in% c("multi:softmax")) {
    params <- list(
      eta = eta,                      
      max_depth = best_max_depth,
      num_class = num_class,
      lambda = lambda
    ) 
    
  } else {
    params <- list(
      eta = eta,                      
      max_depth = best_max_depth,
      lambda = lambda
    )
    
  }
  
  # Training!
  model1 <- xgboost(data = train_matrix,
                    objective = objective,
                    params = params,
                    nrounds = best_nrounds,
                    subsample = subsample,
                    verbose = 0,
                    nthread = 1)
  
  # Get performance score model 1
  
  if (objective %in% c("binary:logistic", "multi:softmax")) {
    
    # Predict on test set using model 1
    predictions <- predict(model1, test_matrix)
    
    # Predictions of binary outcome are probabilities
    if (objective %in% "binary:logistic") {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    } 
    
    # Confusion Matrix
    conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_label))
    
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
    
    
  } else {
    
    
    predictions <- predict(model1, newdata = test_matrix)
    
    # Calculate RMSE
    mod1_metric1 <- rmse(test_label, predictions)
    
    # Calculate R2
    mod1_metric2 <- cor(predictions, test_label)^2
    
  }
  
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]])
  # Creating new feature set, same steps as above
  if (any(sapply(training, is.factor))) {
    model2_train_features <- training[independent]
    model2_train_features <- model.matrix(~ . - 1, data = model2_train_features)
    model2_train_matrix <- xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  } else {
     
    model2_train_features <- training[independent]
    model2_train_matrix <- xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  }
  
  model2 <- xgboost(data = model2_train_matrix,
                    objective = objective,
                    params = params,
                    nrounds = best_nrounds,
                    subsample = subsample,
                    verbose=0,
                    nthread = 1)
  
  if (objective %in% c("binary:logistic", "multi:softmax")) {
    predictions <- predict(model2, test_matrix)
    if (objective %in% "binary:logistic") {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    } 
    conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_label))
    mod2_metric1 <- conf_matrix$overall[1]
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(model2, newdata = test_matrix)
    mod2_metric1 <- rmse(test_label, predictions)
    mod2_metric2 <- cor(predictions, test_label)^2
  }
  
  result <- list()
  result$mod1_metric1 <- mod1_metric1
  result$mod1_metric2 <- mod1_metric2
  result$mod2_metric1 <- mod2_metric1
  result$mod2_metric2 <- mod2_metric2
  result$diff_met1 <- mod1_metric1 - mod2_metric1
  result$diff_met2 <-mod1_metric2 - mod2_metric2
  
  return(result)
}

