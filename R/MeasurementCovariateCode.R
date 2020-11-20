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
  
  # to get table 1 - take source values and then map them - dont map in SQL
  
  # Some SQL to construct the covariate:
  sql <- paste("select c.@row_id_field AS row_id, measurement_concept_id, unit_concept_id,",
               "{@lnAgeInteraction}?{LOG(YEAR(c.cohort_start_date)-p.year_of_birth)*}:{{@ageInteraction}?{(YEAR(c.cohort_start_date)-p.year_of_birth)*}}",
               "{@lnValue}?{LOG(value_as_number)}:{value_as_number} as value_as_number,",
               "measurement_date, abs(datediff(dd, measurement_date, c.cohort_start_date)) as index_time,value_as_number raw_value, YEAR(c.cohort_start_date)-p.year_of_birth as age",
               "from @cdm_database_schema.measurement m inner join @cohort_temp_table c on c.subject_id = m.person_id",
               "and measurement_date >= dateadd(day, @startDay, cohort_start_date) and ",
               "measurement_date <= dateadd(day, @endDay, cohort_start_date)",
               "inner join @cdm_database_schema.person p on p.person_id=c.subject_id",
               "where m.measurement_concept_id in (@concepts) {@lnValue}?{ and value_as_number >0 }"
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
                           lnValue = covariateSettings$lnValue
                           )
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
    dplyr::summarize(covariateValue = max(valueAsNumber),
                     covariateValueSource = max(rawValue))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = min(valueAsNumber),
                       covariateValueSource = min(rawValue))
  } else if(covariateSettings$aggregateMethod == 'mean'){
     covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = mean(valueAsNumber),
                       covariateValueSource = mean(rawValue))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = median(valueAsNumber),
                       covariateValueSource = median(rawValue))
  } else{
    last <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(lastTime = min(indexTime))
    covariates <- merge(covariates,last, 
          by.x = c('rowId','indexTime'), 
          by.y = c('rowId','lastTime') )
    
    covariates <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(covariateValue = mean(valueAsNumber),
                       covariateValueSource = mean(rawValue))
  }
  
  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId
  
  #=================
  # CALCULATE TABLE 1 Measurement info
  table1 <- covariates %>% dplyr::group_by(covariateId) %>%
    dplyr::summarize(meanValue = mean(covariateValueSource), 
                     sdValue = sd(covariateValueSource),
                     count = length(covariateValueSource))
  table1 <- as.data.frame(table1)
  
  covariates <- covariates %>% dplyr::select(rowId, covariateId, covariateValue)
  #=================
  
  
  # impute missing - add age here to be able to input age interaction
  sql <- paste("select distinct c.@row_id_field AS row_id ",
               ", YEAR(c.cohort_start_date)-p.year_of_birth  as age",
                "from @cohort_temp_table c",
               "inner join @cdm_database_schema.person p on p.person_id=c.subject_id")
  
  sql <- SqlRender::render(sql, cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  ppl <- DatabaseConnector::querySql(connection, sql)
  colnames(ppl) <- SqlRender::snakeCaseToCamelCase(colnames(ppl))
  
  
  missingPlp <- ppl[!ppl$rowId%in%covariates$rowId,]
  if(length(missingPlp$rowId)>0){
    
    if(covariateSettings$lnValue){
      covariateSettings$imputationValue <- log(covariateSettings$imputationValue)
    }
    
    if(covariateSettings$ageInteraction){
      covVal <- missingPlp$age*covariateSettings$imputationValue
    } else if(covariateSettings$lnAgeInteraction){
      covVal <- log(missingPlp$age)*covariateSettings$imputationValue
    } else{
      covVal <- covariateSettings$imputationValue
    }
    
    extraData <- data.frame(rowId = missingPlp$rowId, 
                            covariateId = covariateSettings$covariateId, 
                            covariateValue = covVal)
    covariates <- rbind(covariates, extraData[,colnames(covariates)])
  }
  
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   ifelse(covariateSettings$lnValue, 'log(', ''),
                                                   covariateSettings$covariateName,
                                                   ifelse(covariateSettings$lnValue, ')', ''),
                                                   ifelse(covariateSettings$ageInteraction, ' X Age', ''),
                                                   ifelse(covariateSettings$lnAgeInteraction, ' X ln(Age)', '')
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
  
  metaData <- list(sql = sql, call = match.call(), table1 = table1)
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
                            analysisId = analysisId
                            )
  
  attr(covariateSettings, "fun") <- "getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
