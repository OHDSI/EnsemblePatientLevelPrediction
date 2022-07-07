#' Code to run the ensemble model development
#' @param ensembleSettings   The ensemble specifications created using \code{setEnsemble()}
#' @param logSettings        The settings used to specify the logging, created using
#'                           \code{PatientLevelPrediction::createLogSettings()}
#' @param saveDirectory      The location to save the ensemble
#' @param cohortDefinitions  The cohort definitions for cohorts used in the ensemble
#'
#' @export
runEnsemble <- function(ensembleSettings,
                        logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
                        saveDirectory,
                        cohortDefinitions = NULL) {
  
  if(is.null(cohortDefinitions)){
    ids <- unique(
      unlist(
        lapply(
          ensembleSettings$modelDesignList, 
          function(x){c(x$targetId, x$outcomeId)}
        )
      )
    )
    cohortDefinitions <- list()
    length(cohortDefinitions) <- length(ids)
    for(i in 1:length(ids)){
      cohortDefinitions[[i]] <- list(
        id = ids[i], 
        name = paste0('cohort: ',ids[i])
      ) 
    }
  }

  if (ensembleSettings$executionList$extractData) {
    ParallelLogger::logInfo("Extracting data")
    PatientLevelPrediction::runMultiplePlp(
      databaseDetails = ensembleSettings$databaseDetails,
      cohortDefinitions = cohortDefinitions,
      modelDesignList = ensembleSettings$modelDesignList,
      onlyFetchData = T,
      logSettings = logSettings,
      saveDirectory = saveDirectory
    )
  }

  if (ensembleSettings$executionList$trainModels) {
    # make code to run this in parallel?
    ParallelLogger::logInfo("Developing level 1 models")
    PatientLevelPrediction::runMultiplePlp(
      databaseDetails = ensembleSettings$databaseDetails,
      cohortDefinitions = cohortDefinitions,
      modelDesignList = ensembleSettings$modelDesignList,
      onlyFetchData = F,
      logSettings = logSettings,
      saveDirectory = saveDirectory
    )
    
    # load the results
    fileList <- dir(saveDirectory, pattern = "Analysis_", full.names = T)
    
    ensembleSettings$resultList <- lapply(
      fileList,
      function(x){PatientLevelPrediction::loadPlpResult(
        file.path(
          x,
          "plpResult"
        )
      )
      }
    )

   }

  if (ensembleSettings$executionList$createEnsemble) {
    
    if(is.null(ensembleSettings$resultList)){
      stop('No ensemble resultList specified')
    }
    
    # split the predictions Test is using a stacker
    if(attr(ensembleSettings$combinerSettings, "combineFunction") == "learnStacker"){
      ensembleSettings$resultList <- partionTestData(
        resultList = ensembleSettings$resultList,
        ensembleSettings = ensembleSettings$combinerSettings
      )
    }
    
    ParallelLogger::logInfo("Filtering models")
    includeModels <- do.call(
      filterBaseModels, 
      list(
        resultList = ensembleSettings$resultList,
        filterSettings = ensembleSettings$filterSettings
        )
      )
    
    ParallelLogger::logInfo("Learning combination mapping and evaluating")
    ensemble <- do.call(
      createEnsemble, 
      list(
        combinerSettings = ensembleSettings$combinerSettings,
        baseModelResults = includeModels
        )
      )
  }

  if (ensembleSettings$executionList$evaluateEnsemble) {
    # code for applying and evaluating here

    prediction <- do.call(
      applyEnsembleToPredictions,
      list(
        predictionList = lapply(
          ensembleSettings$resultList,
          function(x) x$prediction
        ), 
        ensemble = ensemble
      )
    )

    evaluation <- do.call(
      PatientLevelPrediction::evaluatePlp,
      list(
        prediction = prediction, 
        typeColumn = "evaluationType"
        )
      )

    result <- list(
      model = ensemble,
      prediction = prediction,
      performanceEvaluation = evaluation,
      analysisSettings = list(analysisId = "Ensemble"),
      executionSettings = ensembleSettings$resultList[[1]]$executionSettings
    )
    class(result) <- "plpEnsemble"

    ParallelLogger::logInfo("Saving ensemble")
    saveEnsemble(result, dirPath = file.path(saveDirectory, "Ensemble"))

    return(invisible(result))
    
  } else {

    ParallelLogger::logInfo("Saving ensemble")
    saveEnsembleModel(ensemble, dirPath = file.path(saveDirectory, "Ensemble"))

    return(invisible(ensemble))
  }

}
