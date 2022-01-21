

setEnsemble <- function(
  modelDesignList,
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
  filterSettings,
  combinerSettings
){
  # checking the class of the modelList
  if(class(modelDesignList) == 'modelDesign'){
    modelDesignList <- list(modelDesignList)
  }
  if(sum(unlist(lapply(modelDesignList, function(x) class(x) == 'modelDesign' ))) != length(modelDesignList)){
    stop('Incorrect modelDesignList - must be a list of modelDesign')
  }
  
  # targetId must be the same
  if(length(unique(unlist(lapply(modelDesignList, function(x) x$targetId)))) != 1){
    stop('targetId of each model design must be the same')
  }
  # issues with test if restrictPlpDataSettings different
  if(length(unique(lapply(modelDesignList, function(x) x$restrictPlpDataSettings))) != 1){
    stop('restrictPlpDataSettings of each model design must be the same')
  }
  
  # population settings that exclude must be the same
  valsOfInt <- c('includeAllOutcomes','firstExposureOnly','washoutPeriod',
  'removeSubjectsWithPriorOutcome','priorOutcomeLookback',
  'requireTimeAtRisk','minTimeAtRisk')
  if(length(unique(lapply(modelDesignList, function(x) x$populationSettings[valsOfInt]))) != 1){
    stop('populationSettings that impact target cohort of each model design must be the same')
  }
  
  # set covariateSummary to F
  modelDesignList[[i]]$executeSettings$runCovariateSummary <- F
  
  # check the combinerSettings
  
  return(
    modelDesignList = modelDesignList,
    splitSettings = splitSettings,
    filterSettings = filterSettings,
    combinerSettings = combinerSettings
    )
}

runEnsemble <-function(
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
  
  ParallelLogger::logInfo('Learning combination mapping')
  ensemble <- do.call(
    combineEnsembleFiles, 
    list(
      combinerSettings = ensembleSettings$combinerSettings,
      includeModels = includeModels,
      modelsLocation = saveDirectory
    )
  )
  
  saveEnsemble(ensemble, dirPath = file.path(saveDirectory, 'Ensemble'))
  
  return(invisible(ensemble))
}



# this function takes a list of results and the combination settings 
# to create the ensemble
combineEnsemble <- function(
  resultList,
  combinerSettings
){
  
  fun <- eval(parse(text = attr(combinerSettings, 'combineFunction')))
  
  # this creates a plpModel structure
  ensembleModel <- do.call(fun, 
          list(
            settings = combinerSettings$settings,
            resultList = resultList
            )
          )
  
  prediction <- ensembleModel$prediction
  ensembleModel$prediction <- NULL
  
  model <- list(
    model = list(
      baseModels = lapply(resultList, function(x) x$model),
      ensembleModel = ensembleModel
    ),
    trainDetails = list(analysisId = 'ensemble'),
    settings = list(),
    covariateImportance = data.frame()
  )
  class(model) <- 'ensembleModel'

  performanceEvaluation <- PatientLevelPrediction::evaluatePlp(
    prediction = prediction, 
    typeColumn = 'evaluationType'
    )
  
  result <- list(
    model = ensembleModel,
    prediction = prediction, # of just ensemble or everything?
    performanceEvaluation = performanceEvaluation, # of all single models and ensemble
    analysisSettings = list(analysisId = 'Ensemble'),
    executionSettings = resultList[[1]]$executionSettings
  )
  
  class(result, 'ensemblePlp')
  return(result)
}

# this functions loads the included models and applies the combiner settings
# it learns the combination mapping 
combineEnsembleFiles <- function(
  combinerSettings,
  includeModels,
  modelsLocation 
){
  
  # load each model in the location
  models <- dir(modelsLocation, pattern = 'Analysis_')
  
  models <- models[models %in% includeModels]
  
  if(length(models) > 0 ){
    
    resultList <- lapply(models, 
                        function(x){
                          PatientLevelPrediction::loadPlpResult(
                            file.path(modelsLocation, x, 'plpResult')
                          )
                          }
                        )
    
    ensemble <- combineEnsemble(
      resultList,
      combinerSettings
    )
    
    return(ensemble)
    
  } else{
    return(NULL)
  }
}

# modify this to be for non saved as well
filterEnsembleFiles <- function(
  filterSettings,
  modelsLocation
){
  
  # load each model in the location
  models <- dir(modelsLocation, pattern = 'Analysis_')
  
  includeModels <- c()
  for(model in models){
    resultLocation <- file.path(modelsLocation, model, 'plpResult')
    plpResult <- PatientLevelPrediction::loadPlpResult(resultLocation)
    
    value <- plpResult$performanceEvaluation$evaluationSummary %>%
      dplyr::filter(.data$evaluation == 'cv' && .data$metric == filterSettings$metric)
    
    includeMin <- T
    if(!is.null(filterSettings$minValue)){
      if(value < filterSettings$minValue){
        includeMin <- F
      }
    }
    includeMax <- T
    if(!is.null(filterSettings$maxValue)){
      if(value > filterSettings$maxValue){
        includeMax <- F
      }
    }
    
    if(includeMin && includeMax){
      includeModels <- c(includeModels, model)
    }
    
  }
  
  return(includeModels)
}

saveEnsemble <- function(ensemble, dirPath){
  
  if(!dir.exists(file.path(dirPath, 'ensemble'))){
    dir.create(file.path(dirPath, 'ensemble'), recursive = T)
  }
  
  # save the ensemble
  saveEnsembleModel(
    ensembleModel = ensemble$model, 
    dirPath = file.path(dirPath, 'ensemble')
    )
  
  ensemble$model <- NULL
  saveRDS(ensemble, file = file.path(dirPath, "ensemblePlp.rds"))
  
}

loadEnsemble <- function(dirPath){
  
  ensemble <- readRDS(file = file.path(dirPath, "ensemblePlp.rds"))
  ensemble$model <- loadEnsembleModel(dirPath = file.path(dirPath, 'ensemble'))
  
  return(ensemble)
}


saveEnsembleModel <- function(ensembleModel, dirPath){
  
  if(!dir.exists(file.path(dirPath, 'base'))){
    dir.create(file.path(dirPath, 'base'), recursive = T)
  }
  
  # save the models
  PatientLevelPrediction::savePlpModel(
    plpModel = ensembleModel$model$baseModels[[i]], 
    dirPath = file.path(dirPath, 'base', ensembleModel$model$baseModels[[i]]$trainDetails$analysisId)
    )
  
  #save the ensemble
  baseModels <- lapply(
    1:length(ensembleModel$model$baseModels), 
    function(i){
      file.path(dirPath, 'base', ensembleModel$model$baseModels[[i]]$trainDetails$analysisId)
    } 
  )
  
  class(ensembleModel) <- 'plpModel'
  ensembleModel$model$baseModels <- baseModels
  PatientLevelPrediction::savePlpModel(
    plpModel = ensembleModel, 
    dirPath = file.path(dirPath)
  )
  
}

loadEnsembleModel <- function(dirPath){
  
  # load the ensemble 
  ensembleModel <- PatientLevelPrediction::loadPlpModel(
    dirPath = file.path(dirPath)
  )
  
  # load the base models based on there locations
  ensembleModel$model$baseModels <- lapply(
    ensembleModel$model$baseModels, 
    function(x){
      PatientLevelPrediction::loadPlpModel(dirPath = x)
      }
    )
  
  class(ensembleModel) <- 'ensembleModel'
  return(ensembleModel) 
}
  
  
  