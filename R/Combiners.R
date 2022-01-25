# this function takes a list of results and the combination settings 
# to create the ensemble
combineEnsemble <- function(
  resultList,
  combinerSettings
){
  
  fun <- eval(parse(text = attr(combinerSettings, 'combineFunction')))
  
  # this creates a plpModel structure
  ensembleModel <- do.call(fun, 
                           list(
                             settings = combinerSettings,
                             resultList = resultList
                           )
  )
  
  prediction <- ensembleModel$prediction
  ensembleModel$prediction <- NULL
  
  model <- list(
    model = list(
      baseModels = lapply(resultList, function(x) x$model),
      ensembleModel = ensembleModel # a mapping from list of predictions of base to combined risk
    ),
    trainDetails = list(analysisId = 'ensemble'),
    settings = list(),
    covariateImportance = data.frame()
  )
  class(model) <- 'ensembleModel'
  
  performanceEvaluation <- PatientLevelPrediction::evaluatePlp(
    prediction = prediction, 
    typeColumn = 'evaluationType'
  )
  
  result <- list(
    model = ensembleModel,
    prediction = prediction, # of just ensemble or everything?
    performanceEvaluation = performanceEvaluation, # of all single models and ensemble
    analysisSettings = list(analysisId = 'Ensemble'),
    executionSettings = resultList[[1]]$executionSettings
  )
  
  class(result) <- 'ensemblePlp'
  return(result)
}


# this functions loads the included models and applies the combiner settings
# it learns the combination mapping 
combineEnsembleFiles <- function(
  combinerSettings,
  includeModels,
  modelsLocation 
){
  
  # load each model in the location
  models <- dir(modelsLocation, pattern = 'Analysis_')
  
  models <- models[models %in% includeModels]
  
  if(length(models) > 0 ){
    
    resultList <- lapply(models, 
                         function(x){
                           PatientLevelPrediction::loadPlpResult(
                             file.path(modelsLocation, x, 'plpResult')
                           )
                         }
    )
    
    ensemble <- combineEnsemble(
      resultList,
      combinerSettings
    )
    
    # as models are already saved - replace 
    ensemble$model$model$baseModels <- lapply(
      models,
      function(x){file.path(modelsLocation, x, 'plpResult')}
    )

    return(ensemble)
    
  } else{
    return(NULL)
  }
}

#' Create the settings for a fusion ensemble 
#' @param type  The type of fusion ensemble pick from: 'uniform', 'AUROC', 'AUPRC' or any other metric from evaluationSummary 
#' @param evaluation   The evaluation type used to learn the weights (if evaluation is CV and type is 'AUROC' then the cross validation AUROC is used to determine the weight given to of the base models)
#' @param scaleFunction  How to scale the weights (normalise means weights add up to 1)
#'
#' @examples
#' \dontrun{
#' 
#' comSettingNormAUC <- createFusionCombiner(
#'  type = 'AUROC',
#'  evaluation = 'CV',
#'  scaleFunction = 'normalize'
#' )
#' 
#' comSettingUniform <- createFusionCombiner(
#'  type = 'uniform',
#'  evaluation = 'CV',
#'  scaleFunction = 'normalize'
#' )
#' 
#' }
#' @export
createFusionCombiner <- function(
  type = 'uniform', # 'AUROC', 'AUPRC'
  evaluation = 'CV',
  scaleFunction = 'normalize'
){
  
  settings <- list(
    type = type,
    evaluation = evaluation,
    scaleFunction  = scaleFunction 
  )
  
  attr(settings, 'combineFunction') <- 'learnFusion'
  class(settings) <- 'combinerSettings'
  return(settings)
}

createPredictionMatrix <- function(predictionList, modelNames){
  
  columnsOfInt <- c('outcomeCount','ageYear','gender')
  
  columnsAvailable <- columnsOfInt[columnsOfInt %in% colnames(predictionList[[1]])]
    
  prediction <- predictionList[[1]][,c('rowId', columnsAvailable, 'value')]

  predictionList <- lapply(predictionList, function(x){x %>% dplyr::select(.data$rowId, .data$value)})
  
  if(length(predictionList)>1){
    for(i in 2:length(predictionList)){
      prediction <- merge(prediction, predictionList[[i]], by = 'rowId', all = T)
    }
  }
  
  # set and NA values to 0
  prediction[is.na(prediction)] <- 0
  
  colnames(prediction) <- c('rowId', columnsAvailable, modelNames)
  
  return(prediction)
}

learnFusion <- function(
  settings,
  resultList
){
  
  # get the predictions
  predictions <- lapply(
    resultList, 
    function(x){
      x$prediction %>% 
        dplyr::filter(.data$evaluationType == settings$evaluation)
    }
  )
  
  modelNames <- unlist(
    lapply(
      resultList, 
      function(x){
        x$model$trainDetails$analysisId
      }
    )
  )
  
  predictionDF <- createPredictionMatrix(predictions, modelNames)
  
  if(settings$type == 'uniform'){
    weights <- rep(1, length(predictions))
  } else{
    
    # get the mertics
    weights <- unlist(
      lapply(
        resultList, 
        function(x){
          x$evaluationPerformance$evaluationStatistics %>% 
            dplyr::filter(
              .data$evaluation == settings$evaluation &
              .data$metric == settings$type
            )
        }
      )
    )
  }
  
  scaledWeights <- do.call(eval(parse(text = settings$scaleFunction )), list(weights = weights))
  
  # apply to all prediction evaluations?
  predictionsTest <- lapply(
    resultList, 
    function(x){
      x$prediction %>% 
        dplyr::filter(.data$evaluationType == 'Test')
    }
  )
  predictionDFTest <- createPredictionMatrix(predictionsTest, modelNames)
  predictionDFTest$evaluationType <- 'Test'
  predictionDF$evaluationType <- 'Train'
  
  prediction <- rbind(predictionDF,predictionDFTest)
  prediction$value <- as.matrix(prediction[,modelNames]) %*% as.matrix(scaledWeights)

  attr(prediction, 'metaData') <- attr(resultList[[1]]$prediction, 'metaData')
  
  result <- list(
    ensembleFunction = 'applyFusion',
    ensembleInputs = list(
      modelNames = modelNames, 
      weights = scaledWeights
    ),
    settings = settings
  )
  
  return(
    list(
      ensembleModel = result,
      prediction = prediction
    )
  )
}



# SCALE FUNCTIONS
minmax <- function(weights){
  minVal <- min(weights)
  diffVal <- max(weights) - min(weights)
  newWeights<- (weights - min(weights))/diffVal
  return(newWeights)
}

minNormalize <- function(weights){
  minVal <- min(weights)
  sumVal <- sum((weights - min(weights)))
  newWeights<- (weights - min(weights))/sumVal
  return(newWeights)
}

normalize <- function(weights){
  newWeights<- weights/sum(weights)
  return(newWeights)
}


