getAnalyses <- function(settings, outputFolder,cdmDatabaseName){
  
  cohorts <- system.file("settings", 'CohortsToCreate.csv', package = "SkeletonExistingPredictionModelStudy")
  cohorts <- read.csv(cohorts)
  
    settingLoc <- system.file("settings","settings.csv", package = "SkeletonExistingPredictionModelStudy")
    analysesSettings <- read.csv(settingLoc)
    analysesSettings$modelSettingsId <- 1:nrow(analysesSettings)
    
  analysesSettings$analysisId <- paste0('Analysis_', analysesSettings$analysisId)
  
  # adding extras for shiny
  analysesSettings$cohortName <- analysesSettings$targetCohortName
  analysesSettings$devDatabase <- 'NA'
  analysesSettings$valDatabase <- cdmDatabaseName
  analysesSettings$modelSettingName <- analysesSettings$modelName
  analysesSettings$populationSettingId <- 1
  analysesSettings$covariateSettingId <- analysesSettings$modelName
  
  if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
    dir.create(file.path(outputFolder,cdmDatabaseName))
  }
  write.csv(analysesSettings, file.path(outputFolder,cdmDatabaseName, 'settings.csv'))
  return(analysesSettings)
}


getPopulationSettings <- function(){
  
  runSettingsLoc <- system.file("settings", 'existingModelList.json', package = "SkeletonExistingPredictionModelStudy")
  settings <- ParallelLogger::loadSettingsFromJson(runSettingsLoc )
  
  return(settings$populationSettings)
}


# takes as input model name - then load that json
runModel <- function(modelName, 
                     analysisId,
                     connectionDetails,
                     cohortCovariateDatabaseSchema,
                     cohortCovariateTable,
                     getPlpSettings, 
                     createPopulationSettings){
  
  runSettingsLoc <- system.file("settings", 'existingModelList.json', package = "SkeletonExistingPredictionModelStudy")
  
  settings <- ParallelLogger::loadSettingsFromJson(runSettingsLoc )
  ind <- which(unlist(lapply(settings$modelSettings, function(x) x$details$modelName))==modelName)
  # if ind is empty return no model
  if(length(ind)==0){
    ParallelLogger::logError(paste0('No model called: ',modelName , ' in json settings'))
  }
  runSettings <- processModelJson(settings$modelSetting[[ind]], cohortCovariateDatabaseSchema, cohortCovariateTable)
  
  # create cohort covariates:
  createCovariateCohorts(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = getPlpSettings$cdmDatabaseSchema,
                         vocabularyDatabaseSchema = getPlpSettings$cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortCovariateDatabaseSchema,
                         cohortTable = cohortCovariateTable,
                         oracleTempSchema = getPlpSettings$oracleTempSchema,
                         cohortVarsToCreate = runSettings$cohorts)
  
  
  # extract data
  getPlpSettings$covariateSettings = runSettings$covariateSettings
  plpData <- do.call(PatientLevelPrediction::getPlpData, getPlpSettings)
  
  # get population
  createPopulationSettings$plpData <- plpData
  population <- do.call(PatientLevelPrediction::createStudyPopulation, createPopulationSettings)
  
  # apply model
  plpModel <- getModel(modelName, 
                       analysisId,
                       getPlpSettings$cohortId,
                       createPopulationSettings$outcomeId, 
                       population,
                       runSettings$model)
  
  result <- PatientLevelPrediction::applyModel(population = population, 
                                               plpData = plpData, 
                                               plpModel = plpModel,
                                               calculatePerformance = T)
  
  if(is.null(result)){
    return(NULL)
  }
  
  result$inputSetting$database <- cdmDatabaseName
  result$inputSetting$modelSettings <- list(model = 'existing model', name = modelName)
  result$inputSetting$dataExtrractionSettings$covariateSettings <- runSettings$covariateSettings
  result$inputSetting$populationSettings <- attr(population, "metaData")
  result$executionSummary  <- list()
  result$model <- plpModel
  result$analysisRef <- list()
  result$covariateSummary <- tryCatch({PatientLevelPrediction:::covariateSummary(plpData = plpData, population = population, model = plpModel)},
                                      error = function(e){ParallelLogger::logError(e); return(NULL)})
  
  return(result)
}

getModel <- function(modelName, analysisId,cohortId,outcomeId, population, modelSettings){
  
  predictionFunction <- do.call(paste0('predictFunction.',modelSettings$modelFunction), modelSettings$settings)
  
   
  plpModel <- list(model = modelName,
                   analysisId = analysisId,
                   hyperParamSearch = NULL,
                   index = NULL,
                   trainCVAuc = NULL,
                   modelSettings = list(model = modelName, 
                                        modelParameters = NULL),
                   metaData = NULL,
                   populationSettings = attr(population, "metaData"),
                   trainingTime = NULL,
                   varImp = predictionFunction$varImp,
                   dense = T,
                   cohortId = cohortId,
                   outcomeId = outcomeId,
                   covariateMap = NULL,
                   predict = predictionFunction$predict
  )
  attr(plpModel, "type") <- 'existing'
  class(plpModel) <- 'plpModel'
  
  return(plpModel)
}  
 

# add the functions for the exisitng models here 
#======= add custom function here...
predictFunction.glm <- function(coefficients,
                                finalMapping,
                                predictionType){
  
  finalMapping <- eval(str2lang(paste0(finalMapping, collapse = ' ')))
  
  predictionFunction <- function(plpData, population, coeff = coefficients, type = predictionType){
    
    plpData$covariateData$coefficients <- coeff
    on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)
    
    prediction <- plpData$covariateData$covariates %>% 
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>% 
      dplyr::mutate(values = covariateValue*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>% 
      dplyr::collect() 
    
    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0
    
    # add any final mapping here (e.g., add intercept and mapping)
    prediction$value <- finalMapping(prediction$value)
    
    metaData <- list(predictionType = type,
                     cohortId = attr(population,'metaData')$cohortId,
                     outcomeId = attr(population,'metaData')$outcomeId,
                     timepoint = attr(population,'metaData')$riskWindowEnd)
    
    attr(prediction, "metaData") <- metaData
    
    
    return(prediction)
  }
  
  varImp <- coefficients
  colnames(varImp)[colnames(varImp)=='points'] <- 'covariateValue'
  
  return(list(predict = predictionFunction,
              varImp = varImp))
}