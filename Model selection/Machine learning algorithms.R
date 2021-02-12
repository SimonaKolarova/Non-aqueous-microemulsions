# Generalized linear model
run_logistic_regression <- function(training_testing_data, validation_data, predictors, model_performance, model_name, model_number) {

  # Split TT_data
  trainIndex <- createDataPartition(training_testing_data$Phase.behaviour, p = .7, list = FALSE, times = 1)
  training_data <- training_testing_data[ trainIndex,]
  testing_data <- training_testing_data[ -trainIndex,]
  
  # Train model
  lr_all_pred <- train(training_data[,predictors],
                       training_data$Phase.behaviour, 
                       method = "glm",
                       metric = "ROC",
                       trControl = trainCtrl)

  # Model performance assessement
  model_performance <- model_performance_assessment(testing_data, validation_data, predictors, model_performance, lr_all_pred, model_name, model_number)
  
  return(model_performance)
}  



# Random forest model
run_random_forest <- function(training_testing_data, validation_data, predictors, model_performance, model_name, model_number) {
  
  # Tuning
  mtryValues <- c(round(length(predictors)*1/5),
                  round(length(predictors)*2/5),
                  round(length(predictors)*3/5),
                  round(length(predictors)*4/5),
                  length(predictors))
  
  # Split TT_data
  trainIndex <- createDataPartition(training_testing_data$Phase.behaviour, p = .7, list = FALSE, times = 1)
  training_data <- training_testing_data[ trainIndex,]
  testing_data <- training_testing_data[ -trainIndex,]
  
  # Training model
  rf_all_pred <- train(training_data[,predictors],
                       training_data$Phase.behaviour, 
                       method = "rf",
                       ntree = 1000, 
                       metric = "ROC",
                       tuneGrid = data.frame(mtry = mtryValues),
                       trControl = trainCtrl)
  
  # Model performance assessement
  model_performance <- model_performance_assessment(testing_data, validation_data, predictors, model_performance, rf_all_pred,  model_name, model_number)
  
  return(model_performance)
}  



# Support vector machines model
run_support_vector_machines <- function(training_testing_data, validation_data, predictors, model_performance, model_name, model_number) {
  
  # Split TT_data
  trainIndex <- createDataPartition(training_testing_data$Phase.behaviour, p = .7, list = FALSE, times = 1)
  training_data <- training_testing_data[ trainIndex,]
  testing_data <- training_testing_data[ -trainIndex,]
  
  # Training model
  svm_all_pred <- train(training_data[,predictors],
                       training_data$Phase.behaviour, 
                       method = "svmRadial",
                       metric = "ROC",
                       tuneLength = 12,
                       trControl = trainCtrl)
  
  # Model performance assessement
  model_performance <- model_performance_assessment(testing_data, validation_data, predictors, model_performance, svm_all_pred,  model_name, model_number)
  
  return(model_performance)
}  

# Neural network model

run_neural_network <- function(training_testing_data, validation_data, predictors, model_performance, model_name, model_number) {
  
  # Split TT_data
  trainIndex <- createDataPartition(training_testing_data$Phase.behaviour, p = .7, list = FALSE, times = 1)
  training_data <- training_testing_data[ trainIndex,]
  testing_data <- training_testing_data[ -trainIndex,]
  
  # Training model
  nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
  maxSize <- max(nnetGrid$size)

  nn_all_pred <- train(training_data[,predictors],
                        training_data$Phase.behaviour, 
                        method = "nnet",
                        metric = "ROC",
                        tuneGrid = nnetGrid,
                        trace = FALSE,
                        maxit = 1000,
                        MaxNWts = 1*(maxSize * (length(training_testing_data) + 1) + maxSize + 1),
                        trControl = trainCtrl)
  
  # Model performance assessement
  model_performance <- model_performance_assessment(testing_data, validation_data, predictors, model_performance, nn_all_pred,  model_name, model_number)
  
  return(model_performance)
}  
