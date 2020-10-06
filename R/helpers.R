getAnalyses <- function(settings, outputFolder,cdmDatabaseName){
  
  cohorts <- system.file("settings", 'CohortsToCreate.csv', package = "SkeletonExistingPredictionModelStudy")
  cohorts <- read.csv(cohorts)
  
  if(is.null(settings)){
  cohortsSettings <- cohorts[cohorts$type == 'target', c('cohortId','name')]
  cohortsSettings$outcomeId <- cohorts$cohortId[cohorts$type == 'outcome']
  cohortsSettings$outcomeName <- cohorts$name[cohorts$type == 'outcome']
  colnames(cohortsSettings) <- c('targetId', 'targetName', 'outcomeId', 'outcomeName')
  
  settingLoc <- system.file("settings", package = "SkeletonExistingPredictionModelStudy")
  modelSettings <- data.frame(model = dir(settingLoc, pattern = '_model.csv'))
  modelSettings$modelSettingsId <- 1:nrow(modelSettings)
  analysesSettings <- merge(cohortsSettings, modelSettings)
  } else{
    
    #use data.frame(tId, oId and model) to create...
    settings <- settings[, c('tId', 'oId', 'model')]
    colnames(settings) <- c('targetId','outcomeId','model')
    
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='targetId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'targetName'
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='outcomeId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'outcomeName'
    settings <- settings[,c('targetId', 'targetName', 'outcomeId', 'outcomeName','model')]
    settings$modelSettingsId <- as.double(as.factor(settings$model))
    analysesSettings <- settings
    
  }

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
  varsToCreate <- utils::read.csv(pathToCustom)
  # remove standard covs
  cohortVarsToCreate <- varsToCreate[varsToCreate$type == 'cohortCovariate',]
  measurementVarsToCreate <- varsToCreate[varsToCreate$type == 'measurementCovariate',]
  covSets <- list()
  if(!is.null(standardCovariates)){
    extra <- 1
  } else{
    extra <- 0
    if(nrow(varsToCreate[varsToCreate$type == 'standardCovariate',])!=0){
      warning('Standard covariates used but not set')
    }
  }
  length(covSets) <- nrow(cohortVarsToCreate)+extra + nrow(measurementVarsToCreate)
  
  if(!is.null(standardCovariates)){
    covSets[[1]] <- standardCovariates
  }
  
  for(i in 1:nrow(cohortVarsToCreate)){
    covSets[[extra+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                        analysisId = cohortVarsToCreate$analysisId[i],
                                                        covariateId = cohortVarsToCreate$cohortId[i]*1000+cohortVarsToCreate$analysisId[i],
                                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                                      cohortTable = cohortTable,
                                                      cohortId = cohortVarsToCreate$atlasId[i],
                                                      startDay=cohortVarsToCreate$startDay[i], 
                                                      endDay=cohortVarsToCreate$endDay[i],
                                                      count= ifelse(is.null(cohortVarsToCreate$count), F, cohortVarsToCreate$count[i]), 
                                                      ageInteraction = ifelse(is.null(cohortVarsToCreate$ageInteraction), F, cohortVarsToCreate$ageInteraction[i]),
                                                      lnAgeInteraction = ifelse(is.null(cohortVarsToCreate$lnAgeInteraction), F, cohortVarsToCreate$lnAgeInteraction[i])
                                                      
                                                      )
  }
  
  # add measurement covariates...
  for(i in 1:nrow(measurementVarsToCreate)){
    pathToConcept <- system.file("settings", paste0(measurementVarsToCreate$covariateName[i],'_concepts.csv'), package = "SkeletonExistingPredictionModelStudy")
    conceptSet <- read.csv(pathToConcept)$x
    pathToScaleMap <- system.file("settings", paste0(measurementVarsToCreate$covariateName[i],'_scaleMap.rds'), package = "SkeletonExistingPredictionModelStudy")
    scaleMap <- readRDS(pathToScaleMap)
    
    covSets[[extra+nrow(cohortVarsToCreate)+i]] <- createMeasurementCovariateSettings(covariateName = measurementVarsToCreate$covariateName[i], 
                                                                                      analysisId = measurementVarsToCreate$analysisId[i],
                                                                                      conceptSet = conceptSet,
                                                                                      startDay = measurementVarsToCreate$startDay[i], 
                                                                                      endDay = measurementVarsToCreate$endDay[i], 
                                                                                      scaleMap = scaleMap, 
                                                                                      aggregateMethod = measurementVarsToCreate$aggregateMethod[i],
                                                                                      imputationValue = measurementVarsToCreate$imputationValue[i],
                                                                                      covariateId = measurementVarsToCreate$covariateId[i],
                                                                                      ageInteraction = ifelse(is.null(measurementVarsToCreate$ageInteraction), F, measurementVarsToCreate$ageInteraction[i]),
                                                                                      
                                                                                      lnAgeInteraction = ifelse(is.null(measurementVarsToCreate$lnAgeInteraction), F, measurementVarsToCreate$lnAgeInteraction[i]),
                                                                                      lnValue = ifelse(is.null(measurementVarsToCreate$lnValue), F, measurementVarsToCreate$lnValue[i])
                                                                                      
                                                                                      )
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




