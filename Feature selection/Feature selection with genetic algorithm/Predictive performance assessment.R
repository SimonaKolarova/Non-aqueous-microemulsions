# Predictive performance assessement
model_performance_assessment <- function(training_data, testing_data, predictors, model_performance, model, model_name, model_number) {
  
  # Training data metrics
  auc_training <- as.numeric(auc(
    roc(
      predict(model,training_data[,predictors]), 
      as.numeric(training_data$Phase.behaviour))))
  
  confusion_matrix_training <- confusionMatrix(
    predict(model, training_data[,predictors]),
    training_data$Phase.behaviour, 
    positive = "Microemulsion")
  
  # Testing data metrics
  auc_testing <- as.numeric(auc(
    roc(
      predict(model,testing_data[,predictors]), 
      as.numeric(testing_data$Phase.behaviour))))  
  
  confusion_matrix_testing <- confusionMatrix(
    predict(model, testing_data[,predictors]),
    testing_data$Phase.behaviour, 
    positive = "Microemulsion")
  
  # Add to model_performance dataframe
  model_performance$Model[model_number] <- model_name
  
  model_performance$AUC_training[model_number] <- auc_training
  model_performance$Accuracy_training[model_number] <- as.numeric(confusion_matrix_training[[3]]["Accuracy"])
  model_performance$Kappa_training[model_number] <- as.numeric(confusion_matrix_training[[3]]["Kappa"])
  model_performance$Sensitivity_training[model_number] <- as.numeric(confusion_matrix_training[[4]]["Sensitivity"])
  model_performance$Specificity_training[model_number] <- as.numeric(confusion_matrix_training[[4]]["Specificity"])
  model_performance$Precision_training[model_number] <- as.numeric(confusion_matrix_training[[4]]["Precision"])
  model_performance$Fscore_training[model_number] <- as.numeric(confusion_matrix_training[[4]]["F1"])
  
  model_performance$AUC_testing[model_number] <- auc_testing
  model_performance$Accuracy_testing[model_number] <- as.numeric(confusion_matrix_testing[[3]]["Accuracy"])
  model_performance$Kappa_testing[model_number] <- as.numeric(confusion_matrix_testing[[3]]["Kappa"])
  model_performance$Sensitivity_testing[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Sensitivity"])
  model_performance$Specificity_testing[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Specificity"])
  model_performance$Precision_testing[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Precision"])
  model_performance$Fscore_testing[model_number] <- as.numeric(confusion_matrix_testing[[4]]["F1"])
  
  return(model_performance)
}