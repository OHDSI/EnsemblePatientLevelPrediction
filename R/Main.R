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

#' Execute the Study
#'
#' @details
#' This function executes the SkeletonExistingPredictionModelStudy Study.
#' 
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmDatabaseName      Shareable name of the database 
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target population cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param sampleSize           How many patients to sample from the target population                             
#' @param riskWindowStart      The start of the risk window (in days) relative to the startAnchor.                           
#' @param startAnchor          The anchor point for the start of the risk window. Can be "cohort start" or "cohort end".
#' @param riskWindowEnd        The end of the risk window (in days) relative to the endAnchor parameter
#' @param endAnchor            The anchor point for the end of the risk window. Can be "cohort start" or "cohort end".
#' @param firstExposureOnly    Should only the first exposure per subject be included? Note that this is typically done in the createStudyPopulation function,
#' @param removeSubjectsWithPriorOutcome Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk    Should subject without time at risk be removed?
#' @param minTimeAtRisk        The minimum number of days at risk required to be included
#' @param includeAllOutcomes   (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param standardCovariates   Use this to add standard covariates such as age/gender
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param runAnalyses          Run the model development
#' @param viewShiny            View the results as a shiny app
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                              
#' @param cdmVersion           The version of the common data model                             
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cdmDatabaseName = 'shareable name of the database'
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         outcomeId = 1,
#'         oracleTempSchema = NULL,
#'         riskWindowStart = 1,
#'         startAnchor = 'cohort start',
#'         riskWindowEnd = 365,
#'         endAnchor = 'cohort start',
#'         standardCovariates = FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = T, useDemographicsGender = T),
#'         outputFolder = "c:/temp/study_results", 
#'         createCohorts = T,
#'         runAnalyses = T,
#'         viewShiny = F,
#'         packageResults = F,
#'         minCellCount = 10,
#'         verbosity = "INFO",
#'         cdmVersion = 5)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName = 'friendly database name',
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    oracleTempSchema = cohortDatabaseSchema,
                    sampleSize = NULL,
                    riskWindowStart = 1,
                    startAnchor = 'cohort start',
                    riskWindowEnd = 365,
                    endAnchor = 'cohort start',
                    firstExposureOnly = F,
                    removeSubjectsWithPriorOutcome = F,
                    priorOutcomeLookback = 99999,
                    requireTimeAtRisk = F,
                    minTimeAtRisk = 1,
                    includeAllOutcomes = T,
                    outputFolder,
                    createCohorts = F,
                    runAnalyses = F,
                    viewShiny = F,
                    packageResults = F,
					          minCellCount = 10,
                    verbosity = "INFO",
                    cdmVersion = 5) {
  if (!file.exists(file.path(outputFolder,cdmDatabaseName)))
    dir.create(file.path(outputFolder,cdmDatabaseName), recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder,cdmDatabaseName, "log.txt"))
  
  ## add existing model protocol code?
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  oracleTempSchema = oracleTempSchema,
                  outputFolder = file.path(outputFolder, cdmDatabaseName)) 
  }
  
  if(runAnalyses){
    # add standardCovariates if included 
    standardCovariates <- NULL
    analysisSettings <- getAnalyses(outputFolder,cdmDatabaseName)
    
    for(i in 1:nrow(analysisSettings)){
      
      #getData
      ParallelLogger::logInfo("Extracting data")
      plpData <- getData(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cdmDatabaseName = cdmDatabaseName,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         cohortId = analysisSettings$targetId[i],
                         outcomeId = analysisSettings$outcomeId[i],
                         oracleTempSchema = oracleTempSchema,
                         model = analysisSettings$model[i],
                         standardCovariates = standardCovariates,
                         firstExposureOnly = firstExposureOnly,
                         sampleSize = sampleSize,
                         cdmVersion = cdmVersion)
      
      #create pop
      ParallelLogger::logInfo("Creating population")
      population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, 
                                                                  outcomeId = analysisSettings$outcomeId[i],
                                                                  riskWindowStart = riskWindowStart,
                                                                  startAnchor = startAnchor,
                                                                  riskWindowEnd = riskWindowEnd,
                                                                  endAnchor = endAnchor,
                                                                  firstExposureOnly = firstExposureOnly,
                                                                  removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                  priorOutcomeLookback = priorOutcomeLookback,
                                                                  requireTimeAtRisk = requireTimeAtRisk,
                                                                  minTimeAtRisk = minTimeAtRisk,
                                                                  includeAllOutcomes = includeAllOutcomes)
      
      
      # apply the model:
      plpModel <- list(model = getModel(analysisSettings$model[i]),
                       analysisId = analysisSettings$analysisId[i],
                       hyperParamSearch = NULL,
                       index = NULL,
                       trainCVAuc = NULL,
                       modelSettings = list(model = analysisSettings$model[i], 
                                            modelParameters = NULL),
                       metaData = NULL,
                       populationSettings = attr(population, "metaData"),
                       trainingTime = NULL,
                       varImp = data.frame(covariateId = getModel(analysisSettings$model[i])$covariateId,
                                           covariateValue = getModel(analysisSettings$model[i])$points),
                       dense = T,
                       cohortId = analysisSettings$cohortId[i],
                       outcomeId = analysisSettings$outcomeId[i],
                       covariateMap = NULL,
                       predict = predictExisting(model = analysisSettings$model[i])
      )
      attr(plpModel, "type") <- 'existing'
      class(plpModel) <- 'plpModel'
      ParallelLogger::logInfo("Applying and evaluating model")
      result <- PatientLevelPrediction::applyModel(population = population,
                                                   plpData = plpData,
                                                   plpModel = plpModel)
      
      result$inputSetting$database <- cdmDatabaseName
      result$inputSetting$modelSettings <- list(model = 'existing model', name = analysisSettings$model[i], param = getModel(analysisSettings$model[i]))
      result$inputSetting$dataExtrractionSettings$covariateSettings <- plpData$metaData$call$covariateSettings
      result$inputSetting$populationSettings <- attr(population, "metaData")
      result$executionSummary  <- list()
      result$model <- plpModel
      result$analysisRef <- list()
      result$covariateSummary <- PatientLevelPrediction:::covariateSummary(plpData = plpData, population = population, model = plpModel)
      
      if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
        dir.create(file.path(outputFolder,cdmDatabaseName))
      }
      ParallelLogger::logInfo("Saving results")
      PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i], 'plpResult'))
      ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i])))
    }
    
  }
  
  # [TODO] add create shiny app
  viewer <- TRUE
  if(viewShiny) {
    viewer <- tryCatch({
      PatientLevelPrediction::viewMultiplePlp(file.path(outputFolder,cdmDatabaseName))},
      error = function(e){'No results to view...'})
  }
  
  if (packageResults) {
    ParallelLogger::logInfo("Packaging results")
    packageResults(outputFolder = file.path(outputFolder,cdmDatabaseName),
                   minCellCount = minCellCount)
  }
   
  return(viewer)
}




