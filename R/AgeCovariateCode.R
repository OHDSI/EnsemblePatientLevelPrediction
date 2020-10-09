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

#' Extracts age covariates
#'
#' @details
#' This extracts age mapped covariates
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
getAgeCovariateData <- function(connection,
                                        oracleTempSchema = NULL,
                                        cdmDatabaseSchema,
                                        cdmVersion = "5",
                                        cohortTable = "#cohort_person",
                                        rowIdField = "row_id",
                                        aggregated,
                                        cohortId,
                                        covariateSettings) {
  
  # Some SQL to construct the covariate:
  sql <- paste("select distinct c.@row_id_field AS row_id, ",
               "YEAR(c.cohort_start_date)-p.year_of_birth as value_as_number",
               "from @cohort_temp_table c  ",
               "inner join @cdm_database_schema.person p on p.person_id=c.subject_id"
  )
  
  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema
                           )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  # map data:
  covariates <- covariates[!is.na(covariates$valueAsNumber),]
  covariates$covariateValue <- covariateSettings$ageMap(covariates$valueAsNumber)
  
  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId
  
  covariates <- covariates[,c('rowId','covariateId', 'covariateValue')]

  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Age map:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0)
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "age covariate",
                            domainId = "age covariate",
                            startDay = 0,
                            endDay = 0,
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


createAgeCovariateSettings <- function(covariateName = 'Age at index', 
                                               ageMap = function(x){return(x)},
                                               covariateId = 1458,
                                               analysisId = 458
) {
  
  covariateSettings <- list(covariateName=covariateName, 
                            ageMap=ageMap,
                            covariateId = covariateId, 
                            analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "getAgeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
