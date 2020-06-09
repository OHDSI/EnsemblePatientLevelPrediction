getData <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName,
                    cohortDatabaseSchema,
                    cohortTable,
                    oracleTempSchema,
                    standardCovariates,
                    firstExposureOnly,
                    sampleSize,
                    cdmVersion){
  
  
  pathToCustom <- system.file("settings", 'CustomCovariates.csv', package = "SkeletonExistingPredictionModelStudy")
  cohortVarsToCreate <- utils::read.csv(pathToCustom)
  covSets <- list()
  length(covSets) <- nrow(cohortVarsToCreate)+1
  covSets[[1]] <- standardCovariates
  
  for(i in 1:nrow(cohortVarsToCreate)){
    covSets[[1+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
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
                                     cohortId = 1, 
                                     outcomeIds = 2, 
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


getModel <- function(){
  
  pathToCustom <- system.file("settings", 'SimpleModel.csv', package = "SkeletonExistingPredictionModelStudy")
  coefficients <- utils::read.csv(pathToCustom)
 
   return(coefficients)
}

predictExisting <- function(plpData, population){
  coefficients <- getModel()
  
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
  
  scaleVal <- max(prediction$value)
  if(scaleVal>1){
    prediction$value <- prediction$value/scaleVal
  }
  
  attr(prediction, "metaData") <- list(predictionType = 'binary', scale = scaleVal)
  
  return(prediction)
}




