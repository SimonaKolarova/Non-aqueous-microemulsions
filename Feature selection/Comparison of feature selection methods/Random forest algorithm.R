# Random forest model
run_random_forest <- function(training_testing_data, validation_data, predictors, model_performance, model_name, model_number) {
  
  # Training parameters
  PMsummary <- function(...) c(twoClassSummary(...), defaultSummary(...))
  trainCtrl <- trainControl(method = "cv", number = 10,
                            summaryFunction = PMsummary, classProbs = TRUE, savePredictions=TRUE)
  
  # Tuning
  mtryValues <- c(round(length(predictors)/5),
                  round(length(predictors)*2/5),
                  round(length(predictors)*3/5),
                  round(length(predictors)*4/5),
                  length(predictors))
  
  # Split TT_data
  trainIndex <- createDataPartition(training_testing_data$Phase.behaviour, p = .7, list = FALSE)
  training_data <- training_testing_data[ trainIndex,]
  testing_data <- training_testing_data[ -trainIndex,]
  
  
  # Training model
  rf_model <- train(training_data[,predictors],
                    training_data$Phase.behaviour, 
                    method = "rf",
                    ntree = 50, 
                    metric = "ROC",
                    tuneGrid = data.frame(mtry = mtryValues),
                    importance = TRUE,
                    trControl = trainCtrl)


  # Model performance assessement
  model_performance <- model_performance_assessment(training_data, testing_data, validation_data, predictors, model_performance, rf_model, model_name, model_number)
  
  return(model_performance)
}  