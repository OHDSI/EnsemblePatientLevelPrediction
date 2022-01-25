#' Create setting for Ensemble
#' @param modelDesignList   A list of model designs to develop and then combine. Each model design is created by \code{PatientLevelPrediction::createModelDesign()}
#' @param splitSettings   The test/train and cross validation settings created using \code{PatientLevelPrediction::createDefaultSplitSetting()} or via a custom function
#' @param filterSettings   Setting specifying rules to use to filter (remove) any model specified in the list of model designs that performs insufficiently (these models get ignored from the ensemble)
#' @param combinerSettings  Settings specifying how to combine the remaining models into an ensemble
#'
#' @examples
#' \dontrun{
#' modelDesign1 <- PatientLevelPrediction::createModelDesign(
#'  targetId = 4,
#'  outcomeId = 3, 
#'  restrictPlpDataSettings = restrictPlpDataSettings,
#'  covariateSettings = covSet, 
#'  runCovariateSummary = F,
#'  modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
#'  populationSettings = populationSet, 
#'  preprocessSettings = PatientLevelPrediction::createPreprocessSettings()
#' )

#' modelDesign2 <- PatientLevelPrediction::createModelDesign(
#'  targetId = 4,
#'  outcomeId = 3, 
#'  restrictPlpDataSettings = restrictPlpDataSettings,
#'  covariateSettings = covSet, 
#'  runCovariateSummary = F,
#'  modelSettings = PatientLevelPrediction::setGradientBoostingMachine(),
#'  populationSettings = populationSet, 
#'  preprocessSettings = PatientLevelPrediction::createPreprocessSettings()
#' )
#'
#' ensembleSettings <- setEnsemble(
#'  modelDesignList = list(modelDesign1, modelDesign2),
#'  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
#'  filterSettings = list(minValue = 0.5, maxValue = 1),
#'  combinerSettings = createFusionCombiner(
#'    type = 'uniform',
#'    evaluation = 'CV',
#'    scaleFunction = 'normalize'
#'  )
#')
#' }
#' @export
setEnsemble <- function(
  modelDesignList,
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
  filterSettings,
  combinerSettings
){
  # checking the class of the modelList
  if(class(modelDesignList) == 'modelDesign'){
    modelDesignList <- list(modelDesignList)
  }
  if(sum(unlist(lapply(modelDesignList, function(x) class(x) == 'modelDesign' ))) != length(modelDesignList)){
    stop('Incorrect modelDesignList - must be a list of modelDesign')
  }
  
  # targetId must be the same
  if(length(unique(unlist(lapply(modelDesignList, function(x) x$targetId)))) != 1){
    stop('targetId of each model design must be the same')
  }
  # issues with test if restrictPlpDataSettings different
  if(length(unique(lapply(modelDesignList, function(x) x$restrictPlpDataSettings))) != 1){
    stop('restrictPlpDataSettings of each model design must be the same')
  }
  
  # population settings that exclude must be the same
  valsOfInt <- c('includeAllOutcomes','firstExposureOnly','washoutPeriod',
                 'removeSubjectsWithPriorOutcome','priorOutcomeLookback',
                 'requireTimeAtRisk','minTimeAtRisk')
  if(length(unique(lapply(modelDesignList, function(x) x$populationSettings[valsOfInt]))) != 1){
    stop('populationSettings that impact target cohort of each model design must be the same')
  }
  
  # set covariateSummary to F
  for(i in 1:length(modelDesignList)){
    modelDesignList[[i]]$executeSettings$runCovariateSummary <- F
  }
  # check the combinerSettings
  
  return( 
    list(
      modelDesignList = modelDesignList,
      splitSettings = splitSettings,
      filterSettings = filterSettings,
      combinerSettings = combinerSettings
    )
  )
}