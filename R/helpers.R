getAnalyses <- function(outputFolder,cdmDatabaseName){
  
  
  cohorts <- system.file("settings", 'CohortsToCreate.csv', package = "SkeletonExistingPredictionModelStudy")
  cohorts <- read.csv(cohorts)
  cohortsSettings <- cohorts[cohorts$type == 'target', c('cohortId','name')]
  cohortsSettings$outcomeId <- cohorts$cohortId[cohorts$type == 'outcome']
  cohortsSettings$outcomeName <- cohorts$name[cohorts$type == 'outcome']
  colnames(cohortsSettings) <- c('targetId', 'targetName', 'outcomeId', 'outcomeName')
  
  settingLoc <- system.file("settings", package = "SkeletonExistingPredictionModelStudy")
  modelSettings <- data.frame(model = dir(settingLoc, pattern = '_model.csv'))
  modelSettings$modelSettingsId <- 1:nrow(modelSettings)
  
  analysesSettings <- merge(cohortsSettings, modelSettings)
  analysesSettings$analysisId <- paste0('Analysis_', 1:nrow(analysesSettings))
  
  # adding extras for shiny
  analysesSettings$cohortName <- analysesSettings$targetName
  analysesSettings$devDatabase <- 'NA'
  analysesSettings$valDatabase <- cdmDatabaseName
  analysesSettings$modelSettingName <- analysesSettings$model
  analysesSettings$populationSettingId <- 1
  analysesSettings$covariateSettingId <- analysesSettings$modelSettingsId
  
  if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
    dir.create(file.path(outputFolder,cdmDatabaseName))
  }
  write.csv(analysesSettings, file.path(outputFolder,cdmDatabaseName, 'settings.csv'))
  return(analysesSettings)
}

getData <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName,
                    cohortDatabaseSchema,
                    cohortTable,
                    cohortId,
                    outcomeId,
                    oracleTempSchema,
                    model,
                    standardCovariates,
                    firstExposureOnly,
                    sampleSize,
                    cdmVersion){
  
  
  pathToCustom <- system.file("settings", model, package = "SkeletonExistingPredictionModelStudy")
  cohortVarsToCreate <- utils::read.csv(pathToCustom)
  # remove standard covs
  cohortVarsToCreate <- cohortVarsToCreate[cohortVarsToCreate$atlasId !=0,]
  covSets <- list()
  if(!is.null(standardCovariates)){
    extra <- 1
  } else{
    extra <- 0
  }
  length(covSets) <- nrow(cohortVarsToCreate)+extra
  
  if(!is.null(standardCovariates)){
    covSets[[1]] <- standardCovariates
  }
  
  for(i in 1:nrow(cohortVarsToCreate)){
    covSets[[extra+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                      covariateId = cohortVarsToCreate$cohortId[i]*1000+456,
                                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                                      cohortTable = cohortTable,
                                                      cohortId = cohortVarsToCreate$atlasId[i],
                                                      startDay=cohortVarsToCreate$startDay[i], 
                                                      endDay=cohortVarsToCreate$endDay[i],
                                                      count= ifelse(is.null(cohortVarsToCreate$count), F, cohortVarsToCreate$count[i]), 
                                                      ageInteraction = ifelse(is.null(cohortVarsToCreate$ageInteraction), F, cohortVarsToCreate$ageInteraction[i]))
  }
  
  result <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     oracleTempSchema = oracleTempSchema, 
                                     cohortId = cohortId, 
                                     outcomeIds = outcomeId, 
                                     cohortDatabaseSchema = cohortDatabaseSchema, 
                                     outcomeDatabaseSchema = cohortDatabaseSchema, 
                                     cohortTable = cohortTable, 
                                     outcomeTable = cohortTable, 
                                     cdmVersion = cdmVersion, 
                                     firstExposureOnly = firstExposureOnly, 
                                     sampleSize =  sampleSize, 
                                     covariateSettings = covSets)
  
  return(result)
  
}


getModel <- function(model = 'SimpleModel'){
  
  pathToCustom <- system.file("settings", model, package = "SkeletonExistingPredictionModelStudy")
  coefficients <- utils::read.csv(pathToCustom)
  coefficients <- coefficients[,colnames(coefficients)%in%c('covariateId','points')]
 
   return(coefficients)
}

predictExisting <- function(model){
  
  coefficients <- getModel(model)
  mapping <- getMap(model)
  
  predict <- function(plpData, population){
    
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
    prediction$value <- mapping(prediction$value)
    
    # make sure every value is less than 1 for the evaluatation
    scaleVal <- max(prediction$value)
    if(scaleVal>1){
      prediction$value <- prediction$value/scaleVal
    }
    
    attr(prediction, "metaData") <- list(predictionType = 'binary', scale = scaleVal)
    
    return(prediction)
  }
  
  return(predict)
}




