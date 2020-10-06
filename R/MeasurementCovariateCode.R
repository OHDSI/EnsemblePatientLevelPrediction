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

#' Extracts covariates based on measurements
#'
#' @details
#' This extracts measurement values for a concept set of measurement concept ids
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getMeasurementCovariateData <- function(connection,
                                        oracleTempSchema = NULL,
                                        cdmDatabaseSchema,
                                        cdmVersion = "5",
                                        cohortTable = "#cohort_person",
                                        rowIdField = "row_id",
                                        aggregated,
                                        cohortId,
                                        covariateSettings) {
  
  # Some SQL to construct the covariate:
  sql <- paste("select c.@row_id_field AS row_id, measurement_concept_id, unit_concept_id,",
               "{@lnAgeInteraction}?{LOG(YEAR(c.cohort_start_date)-p.year_of_birth)*}:{{@ageInteraction}?{(YEAR(c.cohort_start_date)-p.year_of_birth)*}}",
               "{@lnValue}?{LOG(value_as_number)}:{value_as_number} as value_as_number,",
               "measurement_date, value_as_number raw_value",
               "from @cdm_database_schema.measurement m inner join @cohort_temp_table c on c.subject_id = m.person_id
   and measurement_date >= dateadd(day, @startDay, cohort_start_date) and 
   measurement_date <= dateadd(day, @endDay, cohort_start_date)",
               "{@ageInteraction | @lnAgeInteraction}?{inner join @cdm_database_schema.person p on p.person_id=c.subject_id}",
               "where m.measurement_concept_id in (@concepts)"
  )
  
  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           concepts = paste(covariateSettings$conceptSet, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           ageInteraction = covariateSettings$ageInteraction,
                           lnAgeInteraction  = covariateSettings$lnAgeInteraction,
                           lnValue = covariateSettings$lnValue)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  # map data:
  covariates <- covariates[!is.na(covariates$valueAsNumber),]
  covariates <- covariateSettings$scaleMap(covariates)
  
  # aggregate data:
  if(covariateSettings$aggregateMethod == 'max'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = max(valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = min(valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'mean'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = mean(valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = median(valueAsNumber))
  } else{
    last <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(lastDate = max(measurementDate))
    
    covariates <- merge(covariates,last, 
                        by.x = c('rowId','measurementDate'), 
                        by.y = c('rowId','lastDate') )
    
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = mean(valueAsNumber))
  }
  
  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId
  
  # impute missing - add age here to be able to input age interaction
  sql <- "select distinct @row_id_field AS row_id from @cohort_temp_table"
  
  sql <- SqlRender::render(sql, cohort_temp_table = cohortTable,
                           row_id_field = rowIdField )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  ppl <- DatabaseConnector::querySql(connection, sql)
  colnames(ppl) <- SqlRender::snakeCaseToCamelCase(colnames(ppl))
  
  
  missingPlp <- ppl$rowId[!ppl$rowId%in%covariates$rowId]
  if(length(missingPlp)>0){
    extraData <- data.frame(rowId = missingPlp, 
                            covariateId = covariateSettings$covariateId, 
                            covariateValue = covariateSettings$imputationValue)
    covariates <- rbind(covariates, extraData[,colnames(covariates)])
  }
  
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0)
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "measurement covariate",
                            domainId = "measurement covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
                            isBinary = "N",
                            missingMeansZero = "Y")
  
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"	
  return(result)
}


createMeasurementCovariateSettings <- function(covariateName, conceptSet,
                                               cohortDatabaseSchema, cohortTable, cohortId,
                                               startDay=-30, endDay=0, 
                                               scaleMap = NULL, aggregateMethod = 'recent',
                                               imputationValue = 0,
                                               ageInteraction = F,
                                               lnAgeInteraction = F,
                                               lnValue = F,
                                               covariateId = 1466,
                                               #measurementId = 1,
                                               analysisId = 466
) {
  
  covariateSettings <- list(covariateName=covariateName, 
                            conceptSet=conceptSet,
                            startDay=startDay,
                            endDay=endDay,
                            scaleMap=scaleMap,
                            aggregateMethod = aggregateMethod,
                            imputationValue = imputationValue,
                            ageInteraction = ageInteraction,
                            lnAgeInteraction = lnAgeInteraction,
                            lnValue = lnValue,
                            covariateId = covariateId,
                            #measurementId = measurementId, 
                            analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
