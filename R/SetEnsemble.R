#' Create setting for creating ensemble from model settings
#' @param modelDesignList    A list of model designs to develop and then combine. Each model design is
#'                           created by \code{PatientLevelPrediction::createModelDesign()}
#' @param databaseDetails    The OMOP CDM database details and connection for extracting the data
#' @param splitSettings      The test/train and cross validation settings created using
#'                           \code{PatientLevelPrediction::createDefaultSplitSetting()} or via a custom
#'                           function
#' @param filterSettings     Setting specifying rules to use to filter (remove) any model specified in
#'                           the list of model designs that performs insufficiently (these models get
#'                           ignored from the ensemble)
#' @param combinerSettings   Settings specifying how to combine the remaining models into an ensemble
#'
#' @examples
#' \dontrun{
#' modelDesign1 <- PatientLevelPrediction::createModelDesign(targetId = 4,
#'                                                           outcomeId = 3,
#'                                                           restrictPlpDataSettings = restrictPlpDataSettings,
#'                                                           covariateSettings = covSet,
#'                                                           runCovariateSummary = F,
#'                                                           modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
#'                                                           populationSettings = populationSet,
#'                                                           preprocessSettings = PatientLevelPrediction::createPreprocessSettings())
#' modelDesign2 <- PatientLevelPrediction::createModelDesign(targetId = 4,
#'                                                           outcomeId = 3,
#'                                                           restrictPlpDataSettings = restrictPlpDataSettings,
#'                                                           covariateSettings = covSet,
#'                                                           runCovariateSummary = F,
#'                                                           modelSettings = PatientLevelPrediction::setGradientBoostingMachine(),
#'                                                           populationSettings = populationSet,
#'                                                           preprocessSettings = PatientLevelPrediction::createPreprocessSettings())
#'
#' ensembleSettings <- setEnsembleFromDesign(modelDesignList = list(modelDesign1, modelDesign2),
#'                                           databaseDetails = PatientLevelPrediction::createDatabaseDetails(),
#'                                           splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
#'                                           filterSettings = list(minValue = 0.5, maxValue = 1),
#'                                           combinerSettings = createFusionCombiner(type = "uniform",
#'                                                                                   evaluation = "CV",
#'                                                                                   scaleFunction = "normalize"))
#' }
#' @export
setEnsembleFromDesign <- function(modelDesignList,
                                  databaseDetails,
                                  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
                                  filterSettings,
                                  combinerSettings) {
  # checking the class of the modelList
  if (class(modelDesignList) == "modelDesign") {
    modelDesignList <- list(modelDesignList)
  }
  if (sum(unlist(lapply(modelDesignList,
                        function(x) class(x) == "modelDesign"))) != length(modelDesignList)) {
    stop("Incorrect modelDesignList - must be a list of modelDesign")
  }

  # targetId must be the same
  if (length(unique(unlist(lapply(modelDesignList, function(x) x$targetId)))) != 1) {
    stop("targetId of each model design must be the same")
  }
  # issues with test if restrictPlpDataSettings different
  if (length(unique(lapply(modelDesignList, function(x) x$restrictPlpDataSettings))) != 1) {
    stop("restrictPlpDataSettings of each model design must be the same")
  }

  # population settings that exclude must be the same
  valsOfInt <- c("includeAllOutcomes",
                 "firstExposureOnly",
                 "washoutPeriod",
                 "removeSubjectsWithPriorOutcome",
                 "priorOutcomeLookback",
                 "requireTimeAtRisk",
                 "minTimeAtRisk")
  if (length(unique(lapply(modelDesignList, function(x) x$populationSettings[valsOfInt]))) != 1) {
    stop("populationSettings that impact target cohort of each model design must be the same")
  }

  # set covariateSummary to F
  for (i in 1:length(modelDesignList)) {
    modelDesignList[[i]]$executeSettings$runCovariateSummary <- F
  }
  # check the combinerSettings

  return(list(executionList = list(extractData = T,
                                   trainModels = T,
                                   createEnsemble = T,
                                   evaluateEnsemble = T),
              databaseDetails = databaseDetails,
              modelDesignList = modelDesignList,
              splitSettings = splitSettings,
              filterSettings = filterSettings,
              combinerSettings = combinerSettings))
}



#' Create setting for creating ensemble from model settings
#' @param resultList         A list of runPlp results to combine.
#' @param filterSettings     Setting specifying rules to use to filter (remove) any model specified in
#'                           the list of model designs that performs insufficiently (these models get
#'                           ignored from the ensemble)
#' @param combinerSettings   Settings specifying how to combine the remaining models into an ensemble
#'
#' @examples
#' \dontrun{
#'
#' plpResult1 <- PatientLevelPrediction::loadPlpResult("./result1")
#' plpResult2 <- PatientLevelPrediction::loadPlpResult("./result2")
#' plpResult3 <- PatientLevelPrediction::loadPlpResult("./result3")
#'
#' ensembleSettings <- setEnsembleFromResults(resultList = list(plpResult1, plpResult2, plpResult3),
#'                                            filterSettings = list(minValue = 0.5, maxValue = 1),
#'                                            combinerSettings = createFusionCombiner(type = "uniform",
#'                                                                                    evaluation = "CV",
#'                                                                                    scaleFunction = "normalize"))
#' }
#' @export
setEnsembleFromResults <- function(resultList, filterSettings, combinerSettings) {
  # checking the class of the modelList
  if (class(resultList) == "runPlp") {
    resultList <- list(resultList)
  }
  if (sum(unlist(lapply(resultList, function(x) class(x) == "runPlp"))) != length(resultList)) {
    stop("Incorrect resultList - must be a list of runPlp objects")
  }


  indexPpl <- lapply(resultList, function(x) x$prediction %>% dplyr::select(.data$subjectId,
                                                                            .data$cohortStartDate))

  # if the models are trained on the same target and index dates we can evaluate otherwise we stop at
  # creating the ensemble
  evaluateEnsemble <- T
  for (i in 1:(length(indexPpl) - 1)) {
    evaluateEnsemble <- evaluateEnsemble && nrow(merge(indexPpl[[i]],
                                                       indexPpl[[i + 1]],
                                                       by = c("subjectId",
                                                              "cohortStartDate"))) == nrow(indexPpl[[i]])
  }

  # check the combinerSettings

  return(list(executionList = list(extractData = F,
                                   trainModels = F,
                                   createEnsemble = T,
                                   evaluateEnsemble = evaluateEnsemble),
              resultList = resultList,
              filterSettings = filterSettings,
              combinerSettings = combinerSettings))
}


#' Create setting for creating ensemble from model settings
#' @param fileVector         A vector of files containing the location of the
#'                           \code{PatientLevelPrediction::runPlp()} results
#' @param filterSettings     Setting specifying rules to use to filter (remove) any model specified in
#'                           the list of model designs that performs insufficiently (these models get
#'                           ignored from the ensemble)
#' @param combinerSettings   Settings specifying how to combine the remaining models into an ensemble
#'
#' @examples
#' \dontrun{
#'
#' ensembleSettings <- setEnsembleFromFiles(fileVector = c("./result1", "./result2", "./result3"),
#'                                          filterSettings = list(minValue = 0.5, maxValue = 1),
#'                                          combinerSettings = createFusionCombiner(type = "uniform",
#'                                                                                  evaluation = "CV",
#'                                                                                  scaleFunction = "normalize"))
#' }
#' @export
setEnsembleFromFiles <- function(fileVector, filterSettings, combinerSettings) {
  # checking the class of the modelList
  if (class(fileVector) != "character") {
    stop("Incorrect fileVector")
  }

  for (i in 1:length(fileVector)) {
    if (!dir.exists(fileVector[i])) {
      stop(paste0("file: ", fileVector[i], " not found"))
    }
  }

  resultList <- lapply(fileVector, function(x) PatientLevelPrediction::loadPlpResult(x))

  # check whether eval is possible?

  # check the combinerSettings

  return(list(executionList = list(extractData = F,
                                   trainModels = F,
                                   createEnsemble = T,
                                   evaluateEnsemble = F),
              resultList = resultList,
              filterSettings = filterSettings,
              combinerSettings = combinerSettings))
}
