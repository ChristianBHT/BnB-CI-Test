
NullGenerator <- function(formula = NULL,
                          data = NULL,
                          p = NULL,
                          objective = "reg:squarederror",
                          nrounds = 100,
                          eta = 0.1,
                          max_depth = 6,
                          num_class = NULL,
                          eps = 1e-15,
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
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  
  data[independent[[1]]] <- sample(data[[independent[1]]]) 
  
  if (objective %in% "reg:squarederror") {
    inTraining <- sample(1:nrow(data), size = floor(p * nrow(data)))
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  } else if (objective %in% c('binary:logistic', 'multi:softprob')) {
    inTraining <- caret::createDataPartition(y = factor(data[[dependent]]), p = p, list = FALSE)
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  }
  
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
  
  if (objective %in% "multi:softprob") {
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
  
  m <- xgboost::xgb.train(data = train_matrix,
                          objective = objective,
                          params = params,
                          nrounds = nrounds,
                          verbose = 0,
                          nthread = nthread)
  
  if (objective %in% "binary:logistic") {
    predictions <- predict(m, test_matrix)  
    log_loss <- -mean(test_label*log(predictions) + (1 - test_label) * log(1 - predictions))
    metric1 <- log_loss
    
    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    
    if (inherits(conf_matrix, "try-error")) {
      metric2 <- NA
    } else {
      metric2 <- conf_matrix$overall[2]
    }
    
    
  } else if (objective %in% "multi:softprob") {
    predictions <- predict(m, test_matrix)  
    pred <- matrix(predictions, ncol=num_class, byrow=TRUE)
    log_loss <- multi_class_log_loss(actual = test_label, predicted = pred)
    metric1 <- log_loss
    
    pred_class <- max.col(pred) - 1
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    
    if (inherits(conf_matrix, "try-error")) {
      metric2 <- NA
    } else {
      metric2 <- conf_matrix$overall[2]
    }
    
    
  } else {
    predictions <- predict(m, test_matrix)
    
    metric1 <- Metrics::rmse(test_label, predictions)
    metric2 <- Metrics::mse(test_label, predictions)
  }

  
  result <- c(as.numeric(metric1), as.numeric(metric2))
  names(result) <- c("Metric1", "Metric2")
  
  return(result)
}

TestGenerator <- function(formula = NULL,
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
  
  if (objective %in% "multi:softmax") {
    objective <- "multi:softprob"
  }
  
  if (!(objective %in% c("multi:softprob", "reg:squarederror", "binary:logistic"))) {
    stop("Only the objective: multi:softprob, multi:softmax, 'reg:squarederror', or 'binary:logistic' are supported.")
  }
  
  
  if (!(class(formula) %in% "formula")) {
    formula <- as.formula(formula)
  }
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  
  if (objective %in% "reg:squarederror") {
    inTraining <- sample(1:nrow(data), size = floor(p * nrow(data)))
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  } else if (objective %in% c('binary:logistic', 'multi:softprob')) {
    inTraining <- caret::createDataPartition(y = factor(data[[dependent]]), p = p, list = FALSE)
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  }
  
  
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
  
  if (objective %in% "multi:softprob") {
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
  
  m <- xgboost::xgb.train(data = train_matrix,
                          objective = objective,
                          params = params,
                          nrounds = nrounds,
                          verbose=0,
                          nthread = nthread)
  
  if (objective %in% "binary:logistic") {
    predictions <- predict(m, test_matrix)  
    log_loss <- -mean(test_label*log(predictions) + (1 - test_label) * log(1 - predictions))
    metric1 <- log_loss
    
    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    
    if (inherits(conf_matrix, "try-error")) {
      metric2 <- NA
    } else {
        metric2 <- conf_matrix$overall[2]
    }
    
  } else if (objective %in% "multi:softprob") {
    predictions <- predict(m, test_matrix)  
    pred <- matrix(predictions, ncol=num_class, byrow=TRUE)
    log_loss <- multi_class_log_loss(actual = test_label, predicted = pred)
    
    metric1 <- log_loss
    
    pred_class <- max.col(pred) - 1
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    
    if (inherits(conf_matrix, "try-error")) {
      metric2 <- NA 
    } else {
      metric2 <- conf_matrix$overall[2]
    }
    
  } else {
    predictions <- predict(m, test_matrix)
    
    metric1 <- Metrics::rmse(test_label, predictions)
    metric2 <- Metrics::mse(test_label, predictions)
  }
  
  result <- c(as.numeric(metric1), as.numeric(metric2))
  names(result) <- c("Metric1", "Metric2")
  return(result)
}




get_pvalues <- function(objective, NullDist, test1_metric, test2_metric) {
  
  NullDist1 <- as.numeric(NullDist[,1])
  NullDist2 <- as.numeric(NullDist[,2])
  test1_metric <- as.numeric(test1_metric)
  test2_metric <- as.numeric(test2_metric)
  
  mean_NullDist1 <- mean(NullDist1)
  mean_NullDist2 <- mean(NullDist2)
  sd_NullDist1 <- sd(NullDist1)
  sd_NullDist2 <- sd(NullDist2)
  
  mean_test1_metric <- mean(test1_metric)
  mean_test2_metric <- mean(test2_metric)
  
  p_value1 <- (sum(NullDist1 <= mean_test1_metric) + 1) / (length(NullDist1) + 1)
  p_value2 <- if (objective %in% 'reg:squarederror') {
    (sum(NullDist2 <= mean_test2_metric) + 1) / (length(NullDist2) + 1)
  } else {
    (sum(NullDist2 >= mean_test2_metric) + 1) / (length(NullDist2) + 1)
  }
  
  Z1 <- (mean_test1_metric - mean_NullDist1) / sd_NullDist1
  Z2 <- (mean_test2_metric - mean_NullDist2) / sd_NullDist2
  
  param_p_value1 <- pnorm(Z1)
  param_p_value2 <- if (objective %in% 'reg:squarederror') {
    pnorm(Z2)
  } else {
    1-pnorm(Z2)
  }
  result <- c(p_value1, p_value2, param_p_value1, param_p_value2)
  names(result) <- c('emp_p_value1', 'emp_p_value2','par_p_value1', 'par_p_value2' )
  return(c(p_value1, p_value2, param_p_value1, param_p_value2))
}



