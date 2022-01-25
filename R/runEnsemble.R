#' Code to run the ensemble model developement (including developing the base models)
#' @param databaseDetails   The OMOP CDM database and connection details created using \code{PatientLevelPrediction::createDatabaseDetails()} 
#' @param ensembleSettings   The ensemble specifications created using \code{setEnsemble()}
#' @param logSettings  The settings used to specify the logging, created using \code{PatientLevelPrediction::createLogSettings()}
#' @param saveDirectory The location to save the ensemble
#'
#' @export
runEnsembleFromDatabase <-function(
  databaseDetails = PatientLevelPrediction::createDatabaseDetails(),
  ensembleSettings,
  logSettings = PatientLevelPrediction::createLogSettings(logName = 'ensemble'),
  saveDirectory
){
  
  ParallelLogger::logInfo('Extracting data')
  PatientLevelPrediction::runMultiplePlp(
    databaseDetails = databaseDetails, 
    modelDesignList = ensembleSettings$modelDesignList, 
    onlyFetchData = T, 
    splitSettings = ensembleSettings$splitSettings, 
    logSettings = logSettings, 
    saveDirectory = saveDirectory
  )
  
  # make code to run this in parallel?
  ParallelLogger::logInfo('Developing level 1 models')
  PatientLevelPrediction::runMultiplePlp(
    databaseDetails = databaseDetails, 
    modelDesignList = ensembleSettings$modelDesignList, 
    onlyFetchData = F, 
    splitSettings = ensembleSettings$splitSettings, 
    logSettings = logSettings, 
    saveDirectory = saveDirectory
  )
  
  ParallelLogger::logInfo('Filtering models')
  includeModels <- do.call(
    filterEnsembleFiles, 
    list(
      filterSettings = ensembleSettings$filterSettings,
      modelsLocation = saveDirectory
    )
  )
  
  ParallelLogger::logInfo('Learning combination mapping and evaluating')
  ensemble <- do.call(
    combineEnsembleFiles, 
    list(
      combinerSettings = ensembleSettings$combinerSettings,
      includeModels = includeModels,
      modelsLocation = saveDirectory
    )
  )
  
  ParallelLogger::logInfo('Saving ensemble')
  saveEnsemble(ensemble, dirPath = file.path(saveDirectory, 'Ensemble'))
  
  return(invisible(ensemble))
}



  
  
  