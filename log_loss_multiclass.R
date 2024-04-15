
multi_class_log_loss <- function(actual, predicted, eps = 1e-15) {
  actual <- factor(actual, levels = unique(actual))
  actual_matrix <- nnet::class.ind(levels(actual))[as.integer(actual),]
  
  clipped_predictions <- pmin(pmax(predicted, eps), 1 - eps)
  
  sum_loss <- -sum(actual_matrix * log(clipped_predictions))
  mean_loss <- sum_loss / nrow(actual_matrix)
  return(mean_loss)
}
