# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonExistingPredictionModelStudy
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


createStudyJson <- function(packageName = 'exampleStudy',
                            packageDescription = 'an example of the skeleton',
                            createdBy,
                            organizationName,
                            settings = data.frame(targetCohortId = ,
                                                  targetCohortName =,
                                                  outcomeId = ,
                                                  outcomeName = 
                                                  ),
                            baseUrl = 'https://...',
                            populationSetting,
                            modelList,
                            outputLocation = './',
                            jsonName = 'existingModelSettings.json'){
  
  json <- list()
  
  json$skeletonType <-  "PatientLevelPredictionExistingStudy"
  json$skeletonVersion <- "v0.0.1"
  json$packageName <- packageName
  json$description <- packageDescription
  
  json$createdBy <- 'add name'
  json$organizationName <-  organizationName
  json$createdDate <- Sys.Date()
  
  json$CohortsToCreate <- unique(data.frame(id = c(settings$targetCohortId,settings$outcomeId),
                                    atlasId = c(settings$targetCohortId,settings$outcomeId),
                                    name = gsub(' ', '_',c(as.character(settings$targetCohortName),as.character(settings$outcomeName)))))
  
  if(!is.null(modelList$details$modelName)){
    models <- modelList$details$modelName
  }else{
    models <- unique(unlist(lapply(1:length(modelList), function(i) modelList[[i]]$details$modelName)))
  }
  settings <- merge(settings,models)
  colnames(settings)[colnames(settings)=='y'] <- 'modelName'
  settings$analysisId <- 1:nrow(settings)
  json$settings <- settings
  
  json$cohortDefinitions  <- getCohorts(settings,
                                   do.call(rbind,lapply(modelList, function(x) x$cohorts)),
                                   baseUrl)
  
  json$populationSettings <- populationSetting
    
  json$modelSettings <- modelList
  
  
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  
  #json <- RJSONIO::toJSON(json)
  ParallelLogger::saveSettingsToJson(json,
                                     file.path(outputLocation, jsonName))
    
return(json)
}

# Insert covariate cohort definitions from ATLAS into package -----------------------
getCohorts <- function(cohorts,
                       covariates,
                       baseUrl = 'https://...'){
  
  # insert the target and outcome cohorts:
  cohortsToCreate <- data.frame(cohortId = unique(c(cohorts$targetCohortId, cohorts$outcomeId, covariates$atlasId)),
                                atlasId = unique(c(cohorts$targetCohortId, cohorts$outcomeId, covariates$atlasId)),
                                name = gsub(' ','_',unique(c(as.character(cohorts$targetCohortName), as.character(cohorts$outcomeName), covariates$cohortName)))
  )
  
  cohortDefinitions <- list()
  length(cohortDefinitions) <- nrow(cohortsToCreate)
  
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Extracting cohort:", cohortsToCreate$name[i]))
    cohortDefinitions[[i]] <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortsToCreate$atlasId[i], 
                                                 baseUrl = baseUrl)
    cohortDefinitions[[i]]$expressionSql <- RJSONIO::toJSON(cohortDefinitions[[i]]$expression,
                                                            digits = 23)
    cohortDefinitions[[i]]$name = cohortsToCreate$name[i]
  }

  return(cohortDefinitions)
}

# Insert models into package -----------------------
createModelJson <- function(modelname = 'SimpleModel', 
                            modelFunction = 'modelFunction.glm',
                          standardCovariates = data.frame(covariateId = c(0003, 1003,
                                                                          2003, 3003,
                                                                          4003, 5003,
                                                                          6003, 7003,
                                                                          8003, 9003,
                                                                          10003, 11003,
                                                                          12003, 13003,
                                                                          14003, 15003,
                                                                          16003, 17003,
                                                                          8507001),
                                                          covariateName = c('Age 0-4', 'Age 5-9',
                                                                            'Age 10-14', 'Age 15-19',
                                                                            'Age 20-24', 'Age 25-30',
                                                                            'Age 30-34', 'Age 35-40',
                                                                            'Age 40-44', 'Age 45-50',
                                                                            'Age 50-54', 'Age 55-60',
                                                                            'Age 60-64', 'Age 65-70',
                                                                            'Age 70-74', 'Age 75-80',
                                                                            'Age 80-84', 'Age 85-90',
                                                                            'Male'), 
                                                          points = c(rep(0,19)),
                                                          offset = c(rep(0,19)),
                                                          power = c(rep(1,19)),
                                                          featureExtraction = c(rep('useDemographicsAgeGroup',18),
                                                                                'useDemographicsGender')),
                          cohortCovariateSettings = list(atlasCovariateIds = c(1,109),
                                                         atlasCovariateNames = c('Testing 1', 'Testing 109'),
                                                         analysisIds = c(456,456),
                                                         startDays = c(-999,-30),
                                                         endDays = c(-1,0),
                                                         points = c(1,2),
                                                         offset = c(rep(0,2)),
                                                         power = c(rep(1,2)),
                                                         count = rep(F, length(points)),
                                                         ageInteraction = rep(F, length(points)),
                                                         lnAgeInteraction = rep(F, length(points))
                          ),
                          
                          measurementCovariateSettings = list(names = c('Measurement 1', 'measurement 2'),
                                                              conceptSets = list(c(435454,64533), c(34343,124453)),
                                                              startDays = c(-90, -30),
                                                              endDays = c(0,0),
                                                              scaleMaps= list(function(x){return(x)}, 
                                                                              function(x){return(0)} ), 
                                                              points = c(1.2,0.6),
                                                              offset = c(rep(0,2)),
                                                              power = c(rep(1,2)),
                                                              aggregateMethods = c('recent', 'max'),
                                                              imputationValues = c(0,0),
                                                              ageInteractions = rep(F, length(points)),
                                                              lnAgeInteractions = rep(F, length(points)),
                                                              lnValues = rep(F, length(points)),
                                                              measurementIds = c(1,2), 
                                                              analysisIds = c(457,457)
                                                              
                                                              
                          ),
                          
                          measurementCohortCovariateSettings = list(names = c('Measurement 1', 'measurement 2'),
                                                                    atlasCovariateIds = c(1,109),
                                                                    types = c('in'),
                                                                    conceptSets = list(c(435454,64533), c(34343,124453)),
                                                                    startDays = c(-90, -30),
                                                                    endDays = c(0,0),
                                                                    scaleMaps= list(function(x){return(x)}, 
                                                                                    function(x){return(0)} ), 
                                                                    points = c(1.2,0.6),
                                                                    offset = c(rep(0,2)),
                                                                    power = c(rep(1,2)),
                                                                    aggregateMethods = c('recent', 'max'),
                                                                    imputationValues = c(0,0),
                                                                    ageInteractions = rep(F, length(points)),
                                                                    lnAgeInteractions = rep(F, length(points)),
                                                                    lnValues = rep(F, length(points)),
                                                                    measurementIds = c(1,2), 
                                                                    analysisIds = c(457,457)
                                                                    
                                                                    
                          ),
                          
                          ageCovariateSettings = list(names = c('log(age)'),
                                                      ageMaps = list(function(x){return(log(x)^2)}),
                                                      ageIds = 1,
                                                      analysisIds = c(458),
                                                      points = c(12.344),
                                                      offset = c(rep(0,1)),
                                                      power = c(rep(1,1))
                                                      
                          ),
                          
                          finalMapping = function(x){return(x)},
                          predictionType = 'binary',
                          baselineHazard = 0.9,
                          offset = 0
){
  
  
  #====================
  #   initiate the settings
  #====================
  
  # create details:
  details <- list(modelName = modelname,
                  author = 'NA',
                  date = Sys.Date())
  
  # initiate the covariates
  covariateSettings <- list()
  
  
  # initiate the model data.frame (this has the covariateId and points)
  model <- list(modelFunction = modelFunction,
                settings = list(finalMapping = finalMapping,
                                predictionType = predictionType,
                                baselineHazard = baselineHazard,
                                offset = offset,
                                coefficients = NULL)
  )
  
  
  #====================
  #   Add Cohort Covariate Settings (if any)
  #====================
  # add atlas cohort covariates 
  if(!is.null(cohortCovariateSettings)){
  
    # check settings
    if(length(cohortCovariateSettings$points) != length(cohortCovariateSettings$atlasCovariateIds)){
      ParallelLogger::logWarn('CovariateSettings points and covariateId length mismatch')
    }
    if(length(cohortCovariateSettings$points) != length(cohortCovariateSettings$offset)){
      ParallelLogger::logWarn('CovariateSettings points and offset length mismatch')
    }
    if(length(cohortCovariateSettings$points) != length(cohortCovariateSettings$power)){
      ParallelLogger::logWarn('CovariateSettings points and power length mismatch')
    }
    
    
    covariateSettings$createCohortCovariateSettings <- lapply(1:length(cohortCovariateSettings$analysisIds), function(i){list(covariateName = paste0(ifelse(cohortCovariateSettings$count[i], ' Number of ', ''),
                                                                                                                                                     cohortCovariateSettings$atlasCovariateNames[i], 
                                                                                                                                                     ifelse(cohortCovariateSettings$ageInteraction[i], ' X Age ', ''),
                                                                                                                                                     ifelse(cohortCovariateSettings$lnAgeInteraction[i], ' X ln(Age) ', ''),
                                                                                                                                                     ' days before: ', cohortCovariateSettings$startDays[i],
                                                                                                                                                     ' days after: ', cohortCovariateSettings$endDays[i]), 
                                                                                                                              covariateId = 1000*cohortCovariateSettings$atlasCovariateIds[i]+cohortCovariateSettings$analysisIds[i],
                                                                                                                              #cohortDatabaseSchema, 
                                                                                                                              #cohortTable, 
                                                                                                                              cohortId = cohortCovariateSettings$atlasCovariateIds[i],
                                                                                                                              startDay = cohortCovariateSettings$startDays[i], 
                                                                                                                              endDay = cohortCovariateSettings$endDays[i], 
                                                                                                                              count = cohortCovariateSettings$count[i], 
                                                                                                                              ageInteraction = cohortCovariateSettings$ageInteraction[i], 
                                                                                                                              lnAgeInteraction = cohortCovariateSettings$lnAgeInteraction[i],
                                                                                                                              analysisId = cohortCovariateSettings$analysisIds[i])})
    
    
    if(modelFunction == 'glm'){
      cmodel <- data.frame(covariateId = 1000*cohortCovariateSettings$atlasCovariateIds+cohortCovariateSettings$analysisIds,
                           points = cohortCovariateSettings$points,
                           offset = cohortCovariateSettings$offset,
                           power = cohortCovariateSettings$power)
      
      model$settings$coefficients <- rbind(model$settings$coefficients , cmodel)
    }
  }
  
  #====================
  #   Add Standard Covariate Settings (if any)
  #====================
  if(!is.null(standardCovariates)){
    
    #FeatureExtraction::createCovariateSettings()
    standFE <- unique(standardCovariates$featureExtraction)
    covariateSettings$createCovariateSettings <- lapply(1:length(standFE), function(i) T)
    names(covariateSettings$createCovariateSettings) <- standFE
    covariateSettings$createCovariateSettings$includedCovariateIds <- unlist(standardCovariates$covariateId)
    
    if(modelFunction == 'glm'){
      model$settings$coefficients  <- rbind(model$settings$coefficients, standardCovariates[,c('covariateId','points', 'offset', 'power')])
    }
    
  }
  
  #====================
  #   Add Measurement Covariate Settings (if any)
  #====================
  # add measurement covariates 
  if(!is.null(measurementCovariateSettings)){
    
    if(length(measurementCovariateSettings$points) != length(measurementCovariateSettings$measurementIds)){
      ParallelLogger::logWarn('measurementCovariateSettings points and covariateId length mismatch')
    }
    if(length(measurementCovariateSettings$points) != length(measurementCovariateSettings$offset)){
      ParallelLogger::logWarn('measurementCovariateSettings points and offset length mismatch')
    }
    if(length(measurementCovariateSettings$points) != length(measurementCovariateSettings$power)){
      ParallelLogger::logWarn('measurementCovariateSettings points and power length mismatch')
    }
    
    covariateSettings$createMeasurementCovariateSettings <- lapply(1:length(measurementCovariateSettings$analysisIds),
                                                                   function(i){
                                                                     list(covariateName = measurementCovariateSettings$names[i], 
                                                                          conceptSet = measurementCovariateSettings$conceptSets[[i]],
                                                                          #cohortDatabaseSchema, 
                                                                          #cohortTable, 
                                                                          #cohortId,
                                                                          startDay = measurementCovariateSettings$startDays[i], 
                                                                          endDay = measurementCovariateSettings$endDays[i], 
                                                                          scaleMap = measurementCovariateSettings$scaleMaps[[i]], 
                                                                          aggregateMethod = measurementCovariateSettings$aggregateMethods[i],
                                                                          imputationValue = measurementCovariateSettings$imputationValues[i],
                                                                          ageInteraction = measurementCovariateSettings$ageInteractions[i],
                                                                          lnAgeInteraction = measurementCovariateSettings$lnAgeInteractions[i],
                                                                          lnValue = measurementCovariateSettings$lnValues[i],
                                                                          covariateId = 1000*measurementCovariateSettings$measurementIds[i]+measurementCovariateSettings$analysisIds[i],
                                                                          analysisId = measurementCovariateSettings$analysisIds[i]
                                                                     )}
    )
    
    if(modelFunction == 'glm'){
      smodel <- data.frame(
        covariateId = 1000*measurementCovariateSettings$measurementIds+measurementCovariateSettings$analysisIds,
        points = measurementCovariateSettings$points,
        offset = measurementCovariateSettings$offset,
        power = measurementCovariateSettings$power
      )
      model$settings$coefficients  <- rbind(model$settings$coefficients , smodel)
    }
    
  }
  
  #====================
  #   Add Measurement Cohort Covariate Settings (if any)
  #====================
  if(!is.null(measurementCohortCovariateSettings)){
    
    if(length(measurementCohortCovariateSettings$points) != length(measurementCohortCovariateSettings$measurementIds)){
      ParallelLogger::logWarn('measurementCohortCovariateSettings points and covariateId length mismatch')
    }
    if(length(measurementCohortCovariateSettings$points) != length(measurementCohortCovariateSettings$offset)){
      ParallelLogger::logWarn('measurementCohortCovariateSettings points and offset length mismatch')
    }
    if(length(measurementCohortCovariateSettings$points) != length(measurementCohortCovariateSettings$power)){
      ParallelLogger::logWarn('measurementCohortCovariateSettings points and power length mismatch')
    }
    
    covariateSettings$createMeasurementCohortCovariateSettings <- lapply(1:length(), function(i){
      list(covariateName = measurementCohortCovariateSettings$names[i], 
           conceptSet = measurementCohortCovariateSettings$conceptSets[[i]],
           #cohortDatabaseSchema, cohortTable,
           cohortId = measurementCohortCovariateSettings$measurementIds[i],
           type = measurementCohortCovariateSettings$types[i],
           startDay = measurementCohortCovariateSettings$startDays[i], 
           endDay = measurementCohortCovariateSettings$endDays[i], 
           scaleMap = measurementCohortCovariateSettings$scaleMaps[[i]], 
           aggregateMethod = measurementCohortCovariateSettings$aggregateMethods[i],
           imputationValue = measurementCohortCovariateSettings$imputationValues[i],
           ageInteraction = measurementCohortCovariateSettings$ageInteractions[i],
           lnAgeInteraction = measurementCohortCovariateSettings$lnAgeInteractions[i],
           lnValue = measurementCohortCovariateSettings$lnValues[i],
           covariateId = 1000*measurementCohortCovariateSettings$measurementIds[i]+measurementCohortCovariateSettings$analysisIds[i],
           analysisId = measurementCohortCovariateSettings$analysisIds[i]
      )})
    
    if(modelFunction == 'glm'){
      scmodel <- data.frame(
        covariateId = 1000*measurementCohortCovariateSettings$measurementIds+measurementCohortCovariateSettings$analysisIds,
        points = measurementCohortCovariateSettings$points,
        offset = measurementCohortCovariateSettings$offset,
        power = measurementCohortCovariateSettings$power
      )
      model$settings$coefficients  <- rbind(model$settings$coefficients , scmodel)
    }
    
  }
  
  #====================
  #   Add Age Covariate Settings (if any)
  #====================
  # add age map variables
  if(!is.null(ageCovariateSettings)){
    
    if(length(ageCovariateSettings$points) != length(ageCovariateSettings$ageIds)){
      ParallelLogger::logWarn('ageCovariateSettings points and covariateId length mismatch')
    }
    if(length(ageCovariateSettings$points) != length(ageCovariateSettings$offset)){
      ParallelLogger::logWarn('ageCovariateSettings points and offset length mismatch')
    }
    if(length(ageCovariateSettings$points) != length(ageCovariateSettings$power)){
      ParallelLogger::logWarn('ageCovariateSettings points and power length mismatch')
    }
    
    covariateSettings$createAgeCovariateSettings <- lapply(1:length(ageCovariateSettings$names), 
                                                           function(i){
                                                             list(covariateName = ageCovariateSettings$names[i], 
                                                                  ageMap = ageCovariateSettings$ageMaps[[i]],
                                                                  covariateId = 1000*ageCovariateSettings$ageIds+ageCovariateSettings$analysisIds[i],
                                                                  analysisId = ageCovariateSettings$analysisIds[i])
                                                           }
    )
    
    if(modelFunction == 'glm'){
      amodel <- data.frame(covariateId = 1000*ageCovariateSettings$ageIds+ageCovariateSettings$analysisIds,
                           points = ageCovariateSettings$points,
                           offset = ageCovariateSettings$offset,
                           power = ageCovariateSettings$power
                           )
      model$settings$coefficients  <- rbind(model$settings$coefficients , amodel)
    }
    
    
  }
  
  
  #====================
  #   insert the cohorts
  #====================
  # insert the custom covariate settings
  cohortsToCreate <- c()
  if(!is.null(cohortCovariateSettings)){
    cohortsToCreate <- data.frame(atlasId = cohortCovariateSettings$atlasCovariateIds, 
                                  cohortName = cohortCovariateSettings$atlasCovariateNames)
  }
  if(!is.null(measurementCohortCovariateSettings)){
    cohortsToCreate <- rbind(cohortsToCreate, 
                             data.frame(atlasId = measurementCohortCovariateSettings$atlasCovariateIds, 
                                        cohortName = measurementCohortCovariateSettings$names)
    )
  }
  
  # data.frame with the requires cohorts 
  cohortsToCreate <- unique(cohortsToCreate)
  
  cohortsToCreate$cohortName <- gsub(' ','_',cohortsToCreate$cohortName)
  
  #if(length(cohortsToCreate) != 0){
  #  for (i in 1:nrow(cohortsToCreate )) {
  #    writeLines(paste("Inserting cohort:", cohortsToCreate$cohortName[i]))
  #    OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], # atlas or cohort? 
  #                                                 name = cohortsToCreate$cohortName[i], 
  #                                                 baseUrl = baseUrl, 
  #                                                 generateStats = F)
  #  }
  #}
  
  #====================
  #  create json and save
  #====================
  settings <- list(details = details,
                   covariateSettings = covariateSettings,
                   model = model,
                   cohorts =  cohortsToCreate)
  
  #saveJsonLocation <- system.file("settings", package = packageName)
  #saveJsonLocation <- file.path(packageDir, 'inst','settings', paste0(gsub(' ', '', modelname),".json"))
  
  #ParallelLogger::saveSettingsToJson(settings,
  #                                   saveJsonLocation)
  
  return(settings)
}



replaceName <- function(packageLocation = getwd(), 
                        packageName = 'ValidateRCRI'){
  
  # rename files:
  #=====
  # file.path(packageLocation,"SkeletonExistingPredictionModelStudy.Rproj")
  # file.path(packageLocation,"R/SkeletonExistingPredictionModelStudy.R")
  filesToRename <- c("SkeletonExistingPredictionModelStudy.Rproj","R/SkeletonExistingPredictionModelStudy.R")
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub("SkeletonExistingPredictionModelStudy", packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  # edit test in files:
  #=====
  # file.path(packageLocation,"DESCRIPTION")
  # file.path(packageLocation,"README.md")
  # file.path(packageLocation,"extras/CodeToRun.R")
  # each file in dir(file.path(packageLocation,"R"))
  
  filesToEdit <- c(file.path(packageLocation,"DESCRIPTION"),
                   file.path(packageLocation,"README.md"),
                   file.path(packageLocation,"extras/CodeToRun.R"),
                   dir(file.path(packageLocation,"R"), full.names = T))
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( "SkeletonExistingPredictionModelStudy", packageName, x )
    cat(y, file=f, sep="\n")
    
  }
}


