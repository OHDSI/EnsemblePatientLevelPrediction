#' Code to run the ensemble model development
#' @param ensembleSettings   The ensemble specifications created using \code{setEnsemble()}
#' @param logSettings        The settings used to specify the logging, created using
#'                           \code{PatientLevelPrediction::createLogSettings()}
#' @param saveDirectory      The location to save the ensemble
#'
#' @export
runEnsemble <- function(ensembleSettings,
                        logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
                        saveDirectory) {

  if (ensembleSettings$executionList$extractData) {
    ParallelLogger::logInfo("Extracting data")
    PatientLevelPrediction::runMultiplePlp(databaseDetails = ensembleSettings$databaseDetails,
                                           modelDesignList = ensembleSettings$modelDesignList,
                                           onlyFetchData = T,
                                           splitSettings = ensembleSettings$splitSettings,
                                           logSettings = logSettings,
                                           saveDirectory = saveDirectory)
  }

  if (ensembleSettings$executionList$trainModels) {
    # make code to run this in parallel?
    ParallelLogger::logInfo("Developing level 1 models")
    PatientLevelPrediction::runMultiplePlp(databaseDetails = ensembleSettings$databaseDetails,
                                           modelDesignList = ensembleSettings$modelDesignList,
                                           onlyFetchData = F,
                                           splitSettings = ensembleSettings$splitSettings,
                                           logSettings = logSettings,
                                           saveDirectory = saveDirectory)

    # load the results
    fileList <- dir(saveDirectory, pattern = "Analysis_", full.names = T)
    ensembleSettings$resultList <- lapply(fileList,
                                          function(x) PatientLevelPrediction::loadPlpResult(file.path(x,
                                                                                                      "plpResult")))
  }

  if (ensembleSettings$executionList$createEnsemble) {

    ParallelLogger::logInfo("Filtering models")
    includeModels <- do.call(filterBaseModels, list(resultList = ensembleSettings$resultList,
                                                    filterSettings = ensembleSettings$filterSettings))

    ParallelLogger::logInfo("Learning combination mapping and evaluating")
    ensemble <- do.call(createEnsemble, list(combinerSettings = ensembleSettings$combinerSettings,
                                             baseModelResults = includeModels))
  }

  if (ensembleSettings$executionList$evaluateEnsemble) {
    # code for applying and evaluating here

    prediction <- do.call(applyEnsembleToPredictions,
                          list(predictionList = lapply(ensembleSettings$resultList,
                                                                                   function(x) x$prediction), ensemble = ensemble))

    evaluation <- do.call(PatientLevelPrediction::evaluatePlp,
                          list(prediction = prediction, typeColumn = "evaluationType"))

    result <- list(model = ensemble,
                   prediction = prediction,
                   performanceEvaluation = evaluation,
                   analysisSettings = list(analysisId = "Ensemble"),
                   executionSettings = ensembleSettings$resultList[[1]]$executionSettings)
    class(result) <- "plpEnsemble"

    ParallelLogger::logInfo("Saving ensemble")
    saveEnsemble(result, dirPath = file.path(saveDirectory, "Ensemble"))

  } else {

    ParallelLogger::logInfo("Saving ensemble")
    saveEnsembleModel(ensemble, dirPath = file.path(saveDirectory, "Ensemble"))

    result <- ensemble
  }

  return(invisible(result))
}
