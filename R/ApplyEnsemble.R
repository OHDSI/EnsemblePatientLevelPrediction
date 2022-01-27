#' Apply an ensembleModel to new data
#' @param ensembleModel        An ensembleModel
#' @param newDatabaseDetails   A databaseDetails object for the new database created using
#'                             \code{PatientLevelPrediction::createDatabaseDetails}
#' @param logSettings          The log settings 
#' @param outputFolder         The location to save the base model results
#'
#' @export
applyEnsemble <- function(ensembleModel,
                          newDatabaseDetails,
                          logSettings = PatientLevelPrediction::createLogSettings(),
                          outputFolder) {

  result <- list()
  length(result) <- length(ensembleModel$model$baseModels)
  # apply each base model
  for (i in 1:length(ensembleModel$model$baseModels)) {
    result[[i]] <- PatientLevelPrediction::externalValidateDbPlp(plpModel = ensembleModel$model$baseModels[[i]],
                                                                 validationDatabaseDetails = newDatabaseDetails,
                                                                 validationRestrictPlpDataSettings = ensembleModel$model$baseModels[[i]]$settings$plpDataSettings,
                                                                 settings = PatientLevelPrediction::createValidationSettings(),
                                                                 logSettings = logSettings,
                                                                 outputFolder = outputFolder)
  }

  # make sure plpModel$trainDetails$analysisId is renamed basemodel_1,...

  # extract predictions:
  predictionList <- lapply(result, function(x) x$prediction)

  ensemblePrediction <- applyEnsembleToPredictions(predictionList, ensembleModel)

  return(ensemblePrediction)

}

#' Apply an ensembleModel to list of base model prediction objects
#' @param predictionList   A list of base model prediction objects
#' @param ensemble         An ensembleModel
#'
#' @export
applyEnsembleToPredictions <- function(predictionList, ensemble) {

  # this has the weights
  ensembleFun <- eval(parse(text = ensemble$model$ensemble$ensembleFunction))

  prediction <- do.call(ensembleFun, list(settings = ensemble$model$ensemble$settings,
                                          predictionList = predictionList))

  return(prediction)
}
