
NullGenerator <- function(formula = NULL,
                          data = NULL,
                          p = NULL,
                          objective = "reg:squarederror",
                          nrounds = 120,
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
                          nrounds = 120,
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



multi_class_log_loss <- function(actual, predicted, eps = 1e-15) {
  actual <- factor(actual, levels = unique(actual))
  actual_matrix <- nnet::class.ind(levels(actual))[as.integer(actual),]
  
  clipped_predictions <- pmin(pmax(predicted, eps), 1 - eps)
  
  sum_loss <- -sum(actual_matrix * log(clipped_predictions))
  mean_loss <- sum_loss / nrow(actual_matrix)
  return(mean_loss)
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
  means <- (p_value1 + p_value2)/2
  
  result <- c(p_value1, p_value2, means)
  names(result) <- c('p_value1', 'p_value2', 'mean')
  
  return(result)
}


test_dist <- function(null, test) {
  NullDist1 <- as.numeric(null[,1])
  NullDist2 <- as.numeric(null[,2])
  TestDist1 <- as.numeric(test[,1])
  TestDist2 <- as.numeric(test[,2])
  
  NullDist1_normal_test <- shapiro.test(NullDist1)
  NullDist2_normal_test <- shapiro.test(NullDist2)
  TestDist1_normal_test <- shapiro.test(TestDist1)
  TestDist2_normal_test <- shapiro.test(TestDist2)
  
  if (NullDist1_normal_test$p.value < 0.05 & TestDist1_normal_test$p.value < 0.05) {
    rmse_mean_test <- wilcox.test(TestDist1, NullDist1, alternative = 'less')
    rmse_pvalue <- rmse_mean_test$p.value
    
  } else {
    rmse_mean_test <- t.test(TestDist1, NullDist1, var.equal = F, alternative = 'less' )
    rmse_pvalue <- rmse_mean_test$p.value
  }
  
  if (NullDist2_normal_test < 0.05) {
    mse_mean_test <- wilcox.test(TestDist2, NullDist2, alternative = 'less')
    mse_pvalue <- mse_mean_test$p.value
    
  } else {
    mse_mean_test <- t.test(TestDist1, NullDist1, var.equal = F, alternative = 'less' )
    mse_pvalue <- mse_mean_test$p.value
  }
  
  return(rmse_pvalue, mse_pvalue)
}

