
# bruk RMSE istenden for P-verdi, og legg til MSE og ut med R^2
# Stratified cross validation?

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
  
  inTraining <- sample(1:nrow(data), size = floor(p * nrow(data)))
  training <- data[inTraining, ]
  testing <- data[-inTraining, ]
  
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

    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- caret::confusionMatrix(as.factor(pred_class), as.factor(test_label))
    
    metric1 <- log_loss
    metric2 <- conf_matrix$overall[2]
    
  } else if (objective %in% "multi:softprob") {
    predictions <- predict(m, test_matrix)  
    pred <- matrix(predictions, ncol=num_class, byrow=TRUE)
    log_loss <- multi_class_log_loss(actual = test_label, predicted = pred)
    
    pred_class <- max.col(pred) - 1
    conf_matrix <- caret::confusionMatrix(as.factor(pred_class), as.factor(test_label))
    
    metric1 <- log_loss
    metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(m, test_matrix)
    
    metric1 <- Metrics::rmse(test_label, predictions)
    metric2 <- Metrics::mse(test_label, predictions)
  }
  
  result <- c(as.numeric(metric1), as.numeric(metric2), m$params$objective, formula, p)
  names(result) <- c("Metric1", "Metric2", "Objective", "Formula", 'P')
  
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
    
    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- caret::confusionMatrix(as.factor(pred_class), as.factor(test_label))
    
    metric1 <- log_loss
    metric2 <- conf_matrix$overall[2]
    
  } else if (objective %in% "multi:softprob") {
    predictions <- predict(m, test_matrix)  
    pred <- matrix(predictions, ncol=num_class, byrow=TRUE)
    log_loss <- multi_class_log_loss(actual = test_label, predicted = pred)
    
    pred_class <- max.col(pred) - 1
    conf_matrix <- caret::confusionMatrix(as.factor(pred_class), as.factor(test_label))
    
    metric1 <- log_loss
    metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(m, test_matrix)
    
    metric1 <- Metrics::rmse(test_label, predictions)
    metric2 <- Metrics::mse(test_label, predictions)
  }
  
  result <- c(as.numeric(metric1), as.numeric(metric2))  
  return(result)
}

# Not used
get_pvalues <- function(null_distr, p_values = 30, p = 0.9) {
  formula = as.formula(as.character(null_distr[1,4]))
  objective = null_distr[1,3]
  
  p_value1 <- list()
  p_value2 <- list()
  param_p_value1 <- list()
  param_p_value2 <- list()
  for (i in 1:p_values) {
    test <- TestGenerator(data = data, formula = formula, p = p)
    
    if (objective %in% c('reg:squarederror')) {
      
      p_value1[i] <- (sum(null_distr[1] <= test[[1]])+1) / (nrow(null_distr)+1)
      p_value2[i] <- (sum(null_distr[2] >= test[[2]])+1) / (nrow(null_distr)+1)
      Z1 <- (test[[1]]-mean(unlist(null_distr[1])))/sd(unlist(null_distr[1]))
      param_p_value1[i] <- pnorm(Z1)
      Z2 <- (test[[2]]-mean(as.numeric(unlist(null_distr[2]))))/sd(as.numeric(unlist(null_distr[2])))
      param_p_value2[i] <- 1-pnorm(Z2)
      
    } else if (objective %in% c('binary:logistic', 'multi:softmax')) {
      
      p_value1[i] <- (sum(null_distr[1] >= test[[1]])+1) / (nrow(null_distr)+1)
      p_value2[i] <- (sum(null_distr[2] >= test[[2]])+1) / (nrow(null_distr)+1)
      Z1 <- (test[[1]]-mean(unlist(null_distr[1])))/sd(unlist(null_distr[1]))
      param_p_value1[i] <- 1 - pnorm(Z1)
      Z2 <- (test[[2]]-mean(as.numeric(unlist(null_distr[2]))))/sd(as.numeric(unlist(null_distr[2])))
      param_p_value2[i] <- 1 - pnorm(Z2)
      
    }
    
    cat(sprintf("Calculating P-values: %d\r", i))
    flush.console()
    Sys.sleep(0.1)
    
  }
  
  p_values_df <- data.frame(
    P_Value1 = p_value1,
    Param_P_Value1 = param_p_value1,
    P_Value2 = p_value2,
    Param_P_Value2 = param_p_value2
  )
  
  return(p_values_df)
}






