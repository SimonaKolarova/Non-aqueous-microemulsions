# Confromal predictions 
run_conformal_prediction <- function(validation_data, model, cp_dataframe, CL, i) {
  
  # Conformal prediction algorithm
  CP <- ConformalClassification$new(confi=CL)
  CP$CalculateCVScores(model=model)
  CP$ClassPredictions <- predict(model$finalModel, newdata = validation_data, predict.all=TRUE)
  CP$CalculatePValues(new.data=validation_data)
  
  # Populating results dataframe
  results_df <- data.frame(matrix(ncol = 5, nrow = nrow(validation_data)))
  colnames(results_df) <- c("Experimental_phase_behaviour", "Microemulsion_psig", "Nonmicroemulsion_psig", "Conformal_prediction", "Type_prediction")
  
  # Saving results to dataframe
  results_df$Experimental_phase_behaviour <- factor(validation_data$Phase.behaviour)
  results_df$Microemulsion_psig <- CP$p.values$Significance_p.values[,1]
  results_df$Nonmicroemulsion_psig <- CP$p.values$Significance_p.values[,2]

  
  # Deducing conformal prediction 
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 1 & results_df$Nonmicroemulsion_psig == 0, "Microemulsion", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 0 & results_df$Nonmicroemulsion_psig == 1, "Nonmicroemulsion", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 1 & results_df$Nonmicroemulsion_psig == 1, "Ambiguous", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 0 & results_df$Nonmicroemulsion_psig == 0, "None", results_df$Conformal_prediction)

  # Deducing prediction type
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Microemulsion" & results_df$Experimental_phase_behaviour == "Microemulsion", "TP", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Nonmicroemulsion" & results_df$Experimental_phase_behaviour == "Nonmicroemulsion", "TN", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Microemulsion" & results_df$Experimental_phase_behaviour == "Nonmicroemulsion", "FP", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Nonmicroemulsion" & results_df$Experimental_phase_behaviour == "Microemulsion", "FN", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Ambiguous", "Ambiguous", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "None", "None", results_df$Type_prediction)
  
  
  # Populating summary dataframe
  cp_dataframe$Confidence_level[i] <- CL
  cp_dataframe$TP[i] <- sum(results_df$Type_prediction == "TP")
  cp_dataframe$TN[i] <- sum(results_df$Type_prediction == "TN")
  cp_dataframe$FP[i] <- sum(results_df$Type_prediction == "FP")
  cp_dataframe$FN[i] <- sum(results_df$Type_prediction == "FN")
  cp_dataframe$Ambiguous[i] <- sum(results_df$Type_prediction == "Ambiguous")
  cp_dataframe$None[i] <- sum(results_df$Type_prediction == "None")

  cp_dataframe$Accuracy[i] <- (cp_dataframe$TP[i]+cp_dataframe$TN[i])/(cp_dataframe$TP[i]+cp_dataframe$TN[i]+cp_dataframe$FP[i]+cp_dataframe$FN[i])
  cp_dataframe$Sensitivity[i] <- cp_dataframe$TP[i]/(cp_dataframe$TP[i]+cp_dataframe$FN[i])
  cp_dataframe$Specificity[i] <- cp_dataframe$TN[i]/(cp_dataframe$TN[i]+cp_dataframe$FP[i])

  return(cp_dataframe)
}


run_conformal_prediction_for_predictions <- function(validation_data, model, CL) {
  
  # Conformal prediction algorithm
  CP <- ConformalClassification$new(confi=CL)
  CP$CalculateCVScores(model=model)
  CP$ClassPredictions <- predict(model$finalModel, newdata = validation_data, predict.all=TRUE)
  CP$CalculatePValues(new.data=validation_data)
  
  # Populating results dataframe
  results_df <- data.frame(matrix(ncol = 12, nrow = nrow(validation_data)))
  colnames(results_df) <- c("PS",	"NPS",	"PS_concentration",	"NPS_concentration",	"SPC_concentration",	
                            "Experimental_phase_behaviour", "Phase_boundary",
                            "Microemulsion_psig", "Nonmicroemulsion_psig", "Aggregate_prediction", 
                            "Conformal_prediction", "Type_prediction")
  
  # Transfering smaple information to dataframe
  results_df$PS <- validation_data$PS
  results_df$NPS <- validation_data$NPS
  results_df$PS_concentration <- validation_data$PS.concentration
  results_df$NPS_concentration <- validation_data$NPS.concentration
  results_df$SPC_concentration <- validation_data$SPC.concentration
  results_df$Experimental_phase_behaviour <- factor(validation_data$Phase.behaviour)
  results_df$Phase_boundary <- validation_data$Phase.boundary
  
  
  # Saving prediction results to dataframe
  results_df$Aggregate_prediction <- CP$ClassPredictions$aggregate
  results_df$Microemulsion_psig <- CP$p.values$Significance_p.values[,1]
  results_df$Nonmicroemulsion_psig <- CP$p.values$Significance_p.values[,2]
  
  
  # Deducing conformal prediction 
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 1 & results_df$Nonmicroemulsion_psig == 0, "Microemulsion", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 0 & results_df$Nonmicroemulsion_psig == 1, "Nonmicroemulsion", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 1 & results_df$Nonmicroemulsion_psig == 1, "Ambiguous", results_df$Conformal_prediction)
  results_df$Conformal_prediction <- ifelse(results_df$Microemulsion_psig == 0 & results_df$Nonmicroemulsion_psig == 0, "Nonconforming", results_df$Conformal_prediction)
  
  # Deducing prediction type
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Microemulsion" & results_df$Experimental_phase_behaviour == "Microemulsion", "True Positive", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Nonmicroemulsion" & results_df$Experimental_phase_behaviour == "Nonmicroemulsion", "True Negative", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Microemulsion" & results_df$Experimental_phase_behaviour == "Nonmicroemulsion", "False Positive", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Nonmicroemulsion" & results_df$Experimental_phase_behaviour == "Microemulsion", "False Negative", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Ambiguous", "Ambiguous", results_df$Type_prediction)
  results_df$Type_prediction <- ifelse(results_df$Conformal_prediction == "Nonconforming", "Nonconforming", results_df$Type_prediction)
  
  # Drop undesired colums
  results_df <- results_df[-c(8:9)]
  
  return(results_df)
}


# Random forest algorithm for conformal classifictaion
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
                    ntree = 1000, 
                    metric = "ROC", 
                    #predict.all=TRUE,
                    #keep.forest=TRUE, 
                    #norm.votes=TRUE,
                    tuneGrid = data.frame(mtry = mtryValues),
                    trControl = trainCtrl)
  
  return(rf_model)
}  