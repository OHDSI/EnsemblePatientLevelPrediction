#' Create the settings for a stacker ensemble - this is an emsemble that learns how to combine 
#'  level 1 models using labelled data
#' @param levelTwoType         The type of level 2 model (linear, GBM, randomForest,...)
#' @param levelTwoHyperparameters The hyperparameter settings for the level 2 model
#' @param levelTwoDataSettings  The settings specifying the data type to use to learn the level 2 model and the proportion of the data  
#'
#' @examples
#' \dontrun{
#'
#' stackerCombine <- createStackerCombiner(
#' levelTwoType = "logisticRegressionStacker",
#' levelTwoHyperparameters = NULL,
#' levelTwoDataSettings = list(type = 'Test', proportion = 0.5)
#' )
#'
#'
#' }
#' @export
createStackerCombiner <- function(
  levelTwoType = "logisticRegression",
  levelTwoHyperparameters = NULL,
  levelTwoDataSettings = list(type = 'CV')
) {
  
  levelTwoDataSettings$seed <- sample(10000,1)
  
  settings <- list(
    levelTwoType = levelTwoType, 
    levelTwoHyperparameters = levelTwoHyperparameters, 
    levelTwoDataSettings = levelTwoDataSettings
    )
  
  attr(settings, "combineFunction") <- "learnStacker"
  class(settings) <- "combinerSettings"
  return(settings)
}



learnStacker <- function(settings, baseModelResults) {
  
  # check there is Test and CV in predictions
  # only checking the first for now (fix this for all)
  if(sum(c('Test', 'CV') %in% unique(baseModelResults[[1]]$prediction$evaluationType)) == 0 ){
    stop('Stacker requires CV or Test types in the predictions')
  }
  
  # get the fitting data predictions
  predictionsFit <- lapply(baseModelResults, function(x) {
    x$prediction %>% dplyr::filter(.data$evaluationType == settings$levelTwoDataSettings$type)
  })
  predictionFitDF <- createPredictionMatrix(predictionsFit)
  
  # fit the stacker 
  stackerModel <- do.call(
    settings$levelTwoType, 
    list(
      hyperparmeters = settings$levelTwoHyperparameters,
      fitData = predictionFitDF
      )
    )
  
  return(
    list(
      ensembleFunction = "applyStackerEnsemble", 
      settings = list(stackerModel = stackerModel)
    )
  )
  
}


applyStackerEnsemble <- function(settings, predictionList) {
  # create prediction with rowId, outcomeCount, evaluationType, ...
  prediction <- createPredictionMatrix(predictionList)
  
  modelNames <- grep("basemodel", colnames(prediction))
  prediction$ensemble <- stats::predict(
    object = settings$stackerModel,
    newdata = prediction[, modelNames],
    type="response"
  )
  
  # now convert to: rowId, outcomeCount, evaluationType, value with evaluationType =
  # modelType+evaluationType
  
  prediction <- tidyr::pivot_longer(data = prediction,
                                    cols = c(modelNames, "ensemble"),
                                    names_to = "modelType",
                                    values_to = "value")
  
  prediction$evaluationType <- paste(prediction$evaluationType, prediction$modelType, sep = "_")
  prediction$modelType <- NULL
  
  attr(prediction, "metaData") <- attr(predictionList[[1]], "metaData")
  
  return(prediction)
}



logisticRegressionStacker <- function(
  hyperparmeters,
  fitData,
  testData
  ){
  
  #levelTwoModel <- stats::glm.fit(
  #  x = fitData[,grep('basemodel',colnames(fitData))], 
  #  y = as.factor(fitData$outcomeCount), 
  #  family = binomial()
  #  )
  
  levelTwoModel <- stats::glm(
    formula = stats::as.formula(
      paste('outcomeCount', 
            paste(
              colnames(fitData)[grep('basemodel',colnames(fitData))], 
              collapse=" + "
            ), 
            sep=" ~ ")
    ), 
    family = stats::binomial(), 
    data = fitData[,c(
      grep('basemodel',colnames(fitData)),
      grep('outcomeCount',colnames(fitData))
      )
      ]
  )
  
  return(levelTwoModel)
  
}

partionTestData <- function(
  resultList,
  ensembleSettings
){
  
  if(ensembleSettings$levelTwoDataSettings$type != 'Test'){
    return(resultList)
  }
  
  ParallelLogger::logInfo('Splitting test data into two sets')
  
  set.seed(ensembleSettings$levelTwoDataSettings$seed)
  
  rowIds <- resultList[[1]]$prediction %>% 
    dplyr::filter(.data$evaluationType == 'Test') %>%
    dplyr::select(.data$rowId)
  
  levelTwoIds <- sample(
    rowIds$rowId, 
    length(rowIds$rowId)*ensembleSettings$levelTwoDataSettings$proportion
    )
  
  ParallelLogger::logInfo(paste0('Using ', length(levelTwoIds), ' rowIds of ',length(rowIds$rowId),' Test rowIds to fit stacker'))
  
  
  for(i in 1:length(resultList)){
    levels(resultList[[i]]$prediction$evaluationType) <- c(levels(resultList[[i]]$prediction$evaluationType), 'levelTwoTest')
    resultList[[i]]$prediction <- resultList[[i]]$prediction %>% 
      dplyr::mutate(
        evaluationType = replace(
          .data$evaluationType, 
          .data$evaluationType == 'Test' & .data$rowId %in% levelTwoIds, 
          'levelTwoTest'
          )
        )
  }
  
  return(resultList)
}

