# this function takes a list of results and the combination settings 
applyEnsemble <- function(
  ensemble,
  newDatabaseDetails,
  logSettings = PatientLevelPrediction::createLogSettings(),
  outputFolder
){
  
  result <- list()
  length(result) <- length(ensemble$model$baseModels)
  # apply each base model
  for(i in 1:length(ensemble$model$baseModels)){
    result[[i]] <- PatientLevelPrediction::externalValidateDbPlp(
      plpModel = ensemble$model$baseModels[[i]], 
      validationDatabaseDetails = newDatabaseDetails, 
      validationRestrictPlpDataSettings = ensemble$model$baseModels[[i]]$settings$plpDataSettings, 
      settings = PatientLevelPrediction::createValidationSettings(), 
      logSettings = logSettings, 
      outputFolder = outputFolder
    )
  }
  
  # make sure plpModel$trainDetails$analysisId is renamed basemodel_1,...
  
  # extract predictions:
  predictionList <- lapply(result, function(x) x$prediction)
  
  ensemblePrediction <- applyEnsembleToPredictions(
    predictionList,
    ensemble
  )
  
  return(ensemblePrediction)
  
}

# to create the ensemble
applyEnsembleToPredictions <- function(
  predictionList,
  ensemble
){
  
  # this has the weights
  ensembleFun <- eval(parse(text = ensemble$model$ensemble$ensembleFunction))

  prediction <- do.call(
    ensembleFun, 
    list(settings = ensemble$model$ensemble$settings,
         predictionList = predictionList
         )
    )
  
  return(prediction)
}