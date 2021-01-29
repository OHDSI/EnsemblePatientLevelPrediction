getAnalyses <- function(settings, outputFolder,cdmDatabaseName){
  
  cohorts <- system.file("settings", 'CohortsToCreate.csv', package = "SkeletonExistingPredictionModelStudy")
  cohorts <- read.csv(cohorts)
  
  if(is.null(settings)){
    cohortsSettings <- cohorts[cohorts$type == 'target', c('cohortId','name')]
    cohortsSettings$outcomeId <- cohorts$cohortId[cohorts$type == 'outcome']
    cohortsSettings$outcomeName <- cohorts$name[cohorts$type == 'outcome']
    colnames(cohortsSettings) <- c('targetId', 'targetName', 'outcomeId', 'outcomeName')
    
    settingLoc <- system.file("settings", package = "SkeletonExistingPredictionModelStudy")
    modelSettings <- gsub('.json', '', data.frame(model = dir(settingLoc, pattern = '.json')))
    modelSettings$modelSettingsId <- 1:nrow(modelSettings)
    analysesSettings <- merge(cohortsSettings, modelSettings)
  } else{
    
    #use data.frame(tId, oId and model) to create...
    settings <- settings[, c('tId', 'oId', 'modelName')]
    colnames(settings) <- c('targetId','outcomeId','modelName')
    
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='targetId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'targetName'
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='outcomeId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'outcomeName'
    settings <- settings[,c('targetId', 'targetName', 'outcomeId', 'outcomeName','modelName')]
    analysesSettings <- settings
    
  }
  analysesSettings$analysisId <- paste0('Analysis_', 1:nrow(analysesSettings))
  
  # adding extras for shiny
  analysesSettings$cohortName <- analysesSettings$targetName
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


# takes as input model name - then load that json
runModel <- function(modelName, 
                     analysisId,
                     connection,
                     cohortCovariateDatabaseSchema,
                     cohortCovariateTable,
                     getPlpSettings, 
                     createPopulationSettings){
  
  runSettingsLoc <- system.file("settings", paste0(modelName,'.json'), package = "SkeletonExistingPredictionModelStudy")
  runSettings <- loadModelJson(runSettingsLoc, cohortCovariateDatabaseSchema, cohortCovariateTable)
  
  # create cohort covariates:
  createCovariateCohorts(connection = connection,
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
  population <- do.call(PatientLevelPrediction::createStudyPopulation, createPopulationSettings)
  
  # apply model
  plpModel <- getModel(modelName, analysisId,getPlpSettings$cohortId,createPopulationSettings$outcomeId, 
                       population,runSettings$model)
  
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
  
  predictionFunction <- do.call(modelSettings$modelFunction, modelSettings$settings)
  
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
  
  predictionFunction <- function(plpData, population){
    
    plpData$covariateData$coefficients <- coefficients
    on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)
    
    prediction <- plpData$covariateData$covariates %>% 
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>% 
      dplyr::mutate(values = covariateValue*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>% dplyr::collect() 
    
    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0
    
    # add any final mapping here (e.g., add intercept and mapping)
    prediction$value <- finalMapping(prediction$value)
    
    attr(prediction, "metaData") <- list(predictionType = predictionType)
    
    return(prediction)
  }
  
  varImp <- coefficients
  colnames(coefficients)[colnames(coefficients)=='points'] <- 'covariateValue'
  
  return(list(predictionFunction = predictionFunction,
              varImp = varImp))
}