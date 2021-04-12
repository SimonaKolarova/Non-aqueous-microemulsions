# Random forest algorithm
run_random_forest <- function(training_testing_data, predictors) {
  
  # Training parameters
  PMsummary <- function(...) c(twoClassSummary(...), defaultSummary(...))
  trainCtrl <- trainControl(method = "cv", 
                            number = 10,
                            summaryFunction = PMsummary, 
                            classProbs = TRUE, 
                            savePredictions=TRUE)
  
  # Tuning
  mtryValues <- c(round(length(predictors)/5),
                  round(length(predictors)*1/5),
                  round(length(predictors)*2/5),
                  round(length(predictors)*3/5),
                  round(length(predictors)*4/5),
                  length(predictors))
  
  # Training model with all training data
  rf_model <- train(training_testing_data[,predictors],
                       training_testing_data$Phase.behaviour, 
                       method = "rf",
                       ntree = 50, 
                       metric = "ROC", 
                       #predict.all=TRUE,
                       #keep.forest=TRUE, 
                       #norm.votes=TRUE,
                       tuneGrid = data.frame(mtry = mtryValues),
                       trControl = trainCtrl)

  return(rf_model)
}  