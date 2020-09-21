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
                                   outcomeId,
                                   outcomeName,
                                   baseUrl = 'https://...'){
  
  # insert the target and outcome cohorts:
  cohortsToCreate <- data.frame(cohortId = c(targetCohortIds, outcomeId),
                                atlasId = c(targetCohortIds, outcomeId),
                                name = c(targetCohortNames, outcomeName),
                                type = c(rep('target', length(targetCohortIds)), 'outcome'))
  
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
                                                            points = c(rep(0,19))),
                            baseUrl = 'https://...',
                            atlasCovariateIds = c(1,109),
                            atlasCovariateNames = c('Testing 1', 'Testing 109'),
                            analysisIds = c(456,456),
                            startDays = c(-999,-30),
                            endDays = c(-1,0),
                            points = c(1,2),
                            count = rep(F, length(points)),
                            ageInteraction = rep(F, length(points))){
  
  # insert the custom covariate settings
  
  #l
  if(file.exists(file.path("./inst/settings",'CustomCovariates.csv' ))){
  cohortsToCreate <- read.csv(file.path("./inst/settings",'CustomCovariates.csv' ))
  cohortsToCreate <- rbind(cohortsToCreate, 
                           data.frame(atlasId = atlasCovariateIds, 
                                cohortName = atlasCovariateNames)
                           )
  } else{
  
  cohortsToCreate <- data.frame(atlasId = atlasCovariateIds, 
                                cohortName = atlasCovariateNames)
  }
  
  cohortsToCreate <- unique(cohortsToCreate)
  
  write.csv(cohortsToCreate, file.path("./inst/settings",'CustomCovariates.csv' ), row.names = F)
  
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$cohortName[i]))
    OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], # atlas or cohort? 
                                                 name = cohortsToCreate$cohortName[i], 
                                                 baseUrl = baseUrl, 
                                                 generateStats = F)
  }
  
  
  # add the model
  model <- data.frame(covariateName = paste0(ifelse(count, ' Number of ', ''),
                                             atlasCovariateNames, 
                                             ifelse(ageInteraction, ' X Age ', ''),
                                             ' days before: ', startDays,
                                             ' days after: ', endDays),
                      cohortId = atlasCovariateIds,
                      atlasId = atlasCovariateIds, 
                      cohortName = atlasCovariateNames,
                      startDay = startDays,
                      endDay = endDays,
                      count = count,
                      ageInteraction = ageInteraction,
                      covariateId = 1000*atlasCovariateIds+analysisIds,
                      points = points)

  if(!is.null(standardCovariates)){
    standardCovariates$cohortId <- 0
    standardCovariates$atlasId <- 0
    standardCovariates$cohortName = 0
    standardCovariates$startDay = 0
    standardCovariates$endDay = 0
    standardCovariates$count = 0
    standardCovariates$ageInteraction = 0
    standardCovariates <- standardCovariates[,colnames(model)]
    model <- rbind(model, standardCovariates)
  }
  write.csv(model, file.path("./inst/settings",paste0(modelname,'_model.csv' )), row.names = F)
  
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
