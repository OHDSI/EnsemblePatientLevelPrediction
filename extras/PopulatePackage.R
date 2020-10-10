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

# Insert covariate cohort definitions from ATLAS into package -----------------------
populatePackageCohorts <- function(targetCohortIds,
                                   targetCohortNames,
                                   outcomeIds,
                                   outcomeNames,
                                   baseUrl = 'https://...'){
  
  # insert the target and outcome cohorts:
  cohortsToCreate <- data.frame(cohortId = c(targetCohortIds, outcomeIds),
                                atlasId = c(targetCohortIds, outcomeIds),
                                name = c(targetCohortNames, outcomeNames),
                                type = c(rep('target', length(targetCohortIds)), rep('outcome',length(outcomeIds)))
  )
  
  write.csv(cohortsToCreate, file.path("./inst/settings",'CohortsToCreate.csv' ), row.names = F)
  
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$name[i]))
    OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], 
                                                 name = cohortsToCreate$name[i], 
                                                 baseUrl = baseUrl, 
                                                 generateStats = F)
  }
  
}

# Insert models into package -----------------------
populatePackageModels <- function(modelname = 'SimpleModel',
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
                                                            featureExtraction = c(rep('useDemographicsAgeGroup',18),
                                                                                  'useDemographicsGender')),
                            cohortCovariateSettings = list(baseUrl = 'https://...',
                                                            atlasCovariateIds = c(1,109),
                                                            atlasCovariateNames = c('Testing 1', 'Testing 109'),
                                                            analysisIds = c(456,456),
                                                            startDays = c(-999,-30),
                                                            endDays = c(-1,0),
                                                            points = c(1,2),
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
                                                                aggregateMethods = c('recent', 'max'),
                                                                imputationValues = c(0,0),
                                                                ageInteractions = rep(F, length(points)),
                                                                lnAgeInteractions = rep(F, length(points)),
                                                                lnValues = rep(F, length(points)),
                                                                measurementIds = c(1,2), 
                                                                analysisIds = c(457,457),
                                                                baseUrl = 'http://'
                                                                
                                                                
                            ),
                            
                            ageCovariateSettings = list(names = c('log(age)'),
                                                        ageMaps = list(function(x){return(log(x)^2)}),
                                                        ageIds = 1,
                                                        analysisIds = c(458),
                                                        points = c(12.344)
                                                        
                                                        ),
                            
                            finalMapping = function(x){return(x)}
                            ){
  
  # insert the custom covariate settings
  
  if(!is.null(cohortCovariateSettings)){
    if(file.exists(file.path("./inst/settings",'CustomCovariates.csv' ))){
      cohortsToCreate <- read.csv(file.path("./inst/settings",'CustomCovariates.csv' ))
      cohortsToCreate <- rbind(cohortsToCreate, 
                               data.frame(atlasId = cohortCovariateSettings$atlasCovariateIds, 
                                          cohortName = cohortCovariateSettings$atlasCovariateNames)
      )
    } else{
      
      cohortsToCreate <- data.frame(atlasId = cohortCovariateSettings$atlasCovariateIds, 
                                    cohortName = cohortCovariateSettings$atlasCovariateNames)
    }
    
    cohortsToCreate <- unique(cohortsToCreate)
    
    write.csv(cohortsToCreate, file.path("./inst/settings",'CustomCovariates.csv' ), row.names = F)
    
    for (i in 1:nrow(cohortsToCreate)) {
      writeLines(paste("Inserting cohort:", cohortsToCreate$cohortName[i]))
      OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], # atlas or cohort? 
                                                   name = cohortsToCreate$cohortName[i], 
                                                   baseUrl = cohortCovariateSettings$baseUrl, 
                                                   generateStats = F)
    }
  }
  
  
  # add the model
  model <- c()
  
  # add atlas cohort covariates 
  if(!is.null(cohortCovariateSettings)){
    cmodel <- data.frame(type = 'cohortCovariate',
                         analysisId = cohortCovariateSettings$analysisIds,
                         covariateName = paste0(ifelse(cohortCovariateSettings$count, ' Number of ', ''),
                                                cohortCovariateSettings$atlasCovariateNames, 
                                                ifelse(cohortCovariateSettings$ageInteraction, ' X Age ', ''),
                                                ifelse(cohortCovariateSettings$lnAgeInteraction, ' X ln(Age) ', ''),
                                                ' days before: ', cohortCovariateSettings$startDays,
                                                ' days after: ', cohortCovariateSettings$endDays),
                         cohortId = cohortCovariateSettings$atlasCovariateIds,
                         atlasId = cohortCovariateSettings$atlasCovariateIds, 
                         cohortName = cohortCovariateSettings$atlasCovariateNames,
                         startDay = cohortCovariateSettings$startDays,
                         endDay = cohortCovariateSettings$endDays,
                         count = cohortCovariateSettings$count,
                         ageInteraction = cohortCovariateSettings$ageInteraction,
                         lnAgeInteraction = cohortCovariateSettings$lnAgeInteraction,
                         lnValue = F,
                         aggregateMethod = 'na',
                         imputationValue = 0,
                         covariateId = 1000*cohortCovariateSettings$atlasCovariateIds+cohortCovariateSettings$analysisIds,
                         points = cohortCovariateSettings$points)
    
    model <- rbind(model, cmodel)
  }

  # add atlas cohort covariates 
  if(!is.null(standardCovariates)){
    
    # save the standard features settings
    write.csv(unique(standardCovariates$featureExtraction),
              file.path(file.path("./inst/settings",paste0(modelname,'_standard_features.csv' ))), row.names = F)
    write.csv(unique(standardCovariates$covariateId),
              file.path(file.path("./inst/settings",paste0(modelname,'_standard_features_include.csv' ))), row.names = F)
    
    
    standardCovariates$type = 'standardCovariate'
    standardCovariates$analysisId = 0
    standardCovariates$cohortId <- 0
    standardCovariates$atlasId <- 0
    standardCovariates$cohortName = 0
    standardCovariates$startDay = 0
    standardCovariates$endDay = 0
    standardCovariates$count = 0
    standardCovariates$ageInteraction = 0
    standardCovariates$lnAgeInteraction = 0
    standardCovariates$lnValue = 0
    standardCovariates$aggregateMethod = 'na'
    standardCovariates$imputationValue = 0
    standardCovariates <- standardCovariates[,colnames(model)]
    model <- rbind(model, standardCovariates)
    
  }
  
  # add measurement covariates 
  if(!is.null(measurementCovariateSettings)){
    
    smodel <- data.frame(
      type = 'measurementCovariate',
      analysisId = measurementCovariateSettings$analysisIds,
      covariateName = measurementCovariateSettings$names,
      cohortId = 0,
      atlasId = 0, 
      cohortName = 'na',
      startDay = measurementCovariateSettings$startDays,
      endDay = measurementCovariateSettings$endDays,
      count = 0,
      ageInteraction = measurementCovariateSettings$ageInteractions,
      lnAgeInteraction = measurementCovariateSettings$lnAgeInteractions,
      lnValue = measurementCovariateSettings$lnValues,
      aggregateMethod = measurementCovariateSettings$aggregateMethods,
      imputationValue = measurementCovariateSettings$imputationValues,
      covariateId = 1000*measurementCovariateSettings$measurementIds+measurementCovariateSettings$analysisIds,
      points = measurementCovariateSettings$points
      )
    smodel <- smodel[,colnames(model)]
    model <- rbind(model, smodel)
    
    
    # save concept sets and scale mappers
    for(i in 1:length(measurementCovariateSettings$names)){
      write.csv(measurementCovariateSettings$conceptSets[[i]],
                file.path(file.path("./inst/settings",paste0(measurementCovariateSettings$names[i],'_concepts.csv' ))), row.names = F)
      
      saveRDS(measurementCovariateSettings$scaleMaps[[i]], 
              file.path(file.path("./inst/settings",paste0(measurementCovariateSettings$names[i],'_scaleMap.rds' ))))
    }
    
  }
  
  if(!is.null(measurementCohortCovariateSettings)){
    
    scmodel <- data.frame(
      type = paste0('measurementCohortCovariate_',measurementCohortCovariateSettings$types),
      analysisId = measurementCohortCovariateSettings$analysisIds,
      covariateName = measurementCohortCovariateSettings$names,
      cohortId = measurementCohortCovariateSettings$atlasCovariateIds,
      atlasId = measurementCohortCovariateSettings$atlasCovariateIds, 
      cohortName = 'na',
      startDay = measurementCohortCovariateSettings$startDays,
      endDay = measurementCohortCovariateSettings$endDays,
      count = 0,
      ageInteraction = measurementCohortCovariateSettings$ageInteractions,
      lnAgeInteraction = measurementCohortCovariateSettings$lnAgeInteractions,
      lnValue = measurementCohortCovariateSettings$lnValues,
      aggregateMethod = measurementCohortCovariateSettings$aggregateMethods,
      imputationValue = measurementCohortCovariateSettings$imputationValues,
      covariateId = 1000*measurementCohortCovariateSettings$measurementIds+measurementCohortCovariateSettings$analysisIds,
      points = measurementCohortCovariateSettings$points
    )
    scmodel <- scmodel[,colnames(model)]
    model <- rbind(model, scmodel)
    
    
    # save concept sets and scale mappers
    for(i in 1:length(measurementCohortCovariateSettings$names)){
      write.csv(measurementCohortCovariateSettings$conceptSets[[i]],
                file.path(file.path("./inst/settings",paste0(measurementCohortCovariateSettings$names[i],'_concepts.csv' ))), row.names = F)
      
      saveRDS(measurementCohortCovariateSettings$scaleMaps[[i]], 
              file.path(file.path("./inst/settings",paste0(measurementCohortCovariateSettings$names[i],'_scaleMap.rds' ))))
    }
    
    # add cohorts
    newCohortsToCreate <- data.frame(atlasId = measurementCohortCovariateSettings$atlasCovariateIds, 
                                  cohortName = measurementCohortCovariateSettings$names)
    
    newCohortsToCreate <- unique(newCohortsToCreate)
    
    if(file.exists(file.path("./inst/settings",'CustomCovariates.csv' ))){
      cohortsToCreate <- read.csv(file.path("./inst/settings",'CustomCovariates.csv' ))
      cohortsToCreate <- rbind(cohortsToCreate, 
                               newCohortsToCreate)
    } else{
      
      cohortsToCreate <- newCohortsToCreate  
      }
    
    cohortsToCreate <- unique(cohortsToCreate)
    
    write.csv(cohortsToCreate, file.path("./inst/settings",'CustomCovariates.csv' ), row.names = F)
    
    
    for (i in 1:nrow(newCohortsToCreate)) {
      writeLines(paste("Inserting cohort:", newCohortsToCreate$cohortName[i]))
      OhdsiRTools::insertCohortDefinitionInPackage(definitionId = newCohortsToCreate$atlasId[i], # atlas or cohort? 
                                                   name = newCohortsToCreate$cohortName[i], 
                                                   baseUrl = measurementCohortCovariateSettings$baseUrl, 
                                                   generateStats = F)
    }
    
  }
  
  # add age map variables
  if(!is.null(ageCovariateSettings)){
    
    amodel <- data.frame(
      type = 'ageCovariate',
      analysisId = ageCovariateSettings$analysisIds,
      covariateName = ageCovariateSettings$names,
      cohortId = 0,
      atlasId = 0, 
      cohortName = 'na',
      startDay = 0,
      endDay = 0,
      count = 0,
      ageInteraction = F,
      lnAgeInteraction = F,
      lnValue = F,
      aggregateMethod = 'na',
      imputationValue = 0,
      covariateId = 1000*ageCovariateSettings$ageIds+ageCovariateSettings$analysisIds,
      points = ageCovariateSettings$points
    )
    amodel <- amodel[,colnames(model)]
    model <- rbind(model, amodel)
    
    for(i in 1:length(ageCovariateSettings$names)){
      saveRDS(ageCovariateSettings$ageMaps[[i]], 
              file.path(file.path("./inst/settings",paste0(gsub(' ', '_',gsub('\\)','_',gsub('\\(','_',ageCovariateSettings$names[i]))),'_ageMap.rds' ))))
    }
    
  }
  
  write.csv(model, file.path("./inst/settings",paste0(modelname,'_model.csv' )), row.names = F)
  
  
  # save final mapping function
  saveRDS(finalMapping, 
          file.path(file.path("./inst/settings",paste0(modelname,'_finalMap.rds' ))))
  
  return(TRUE)
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
