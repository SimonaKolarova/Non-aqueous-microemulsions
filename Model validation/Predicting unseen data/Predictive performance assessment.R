# Predictive performance assessement
model_performance_assessment <- function(validation_data, model_performance, model, subset_name, model_number) {
  
  # Data metrics
  auc <- as.numeric(auc(
    roc(predict(model, validation_data), 
    as.numeric(validation_data$Phase.behaviour))))
  
  confusion_matrix<- confusionMatrix(
    predict(model, validation_data),
    validation_data$Phase.behaviour, 
    positive = "Microemulsion")
  
  
  # Add to model_performance dataframe
  model_performance$Validation_data[model_number] <- subset_name
  
  model_performance$AUC[model_number] <- auc
  model_performance$Accuracy[model_number] <- as.numeric(confusion_matrix[[3]]["Accuracy"])
  model_performance$Kappa[model_number] <- as.numeric(confusion_matrix[[3]]["Kappa"])
  model_performance$Sensitivity[model_number] <- as.numeric(confusion_matrix[[4]]["Sensitivity"])
  model_performance$Specificity[model_number] <- as.numeric(confusion_matrix[[4]]["Specificity"])
  model_performance$Precision[model_number] <- as.numeric(confusion_matrix[[4]]["Precision"])
  model_performance$Fscore[model_number] <- as.numeric(confusion_matrix[[4]]["F1"])

  
  return(model_performance)
}