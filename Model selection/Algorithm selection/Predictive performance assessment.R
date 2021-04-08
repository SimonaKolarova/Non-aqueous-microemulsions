# Predictive performance assessement
model_performance_assessment <- function(testing_data, predictors, model_performance, model, model_name, model_number) {
  
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
  
  model_performance$AUC[model_number] <- auc_testing
  model_performance$Accuracy[model_number] <- as.numeric(confusion_matrix_testing[[3]]["Accuracy"])
  model_performance$Kappa[model_number] <- as.numeric(confusion_matrix_testing[[3]]["Kappa"])
  model_performance$Sensitivity[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Sensitivity"])
  model_performance$Specificity[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Specificity"])
  model_performance$Precision[model_number] <- as.numeric(confusion_matrix_testing[[4]]["Precision"])
  model_performance$Fscore[model_number] <- as.numeric(confusion_matrix_testing[[4]]["F1"])
  
  return(model_performance)
}