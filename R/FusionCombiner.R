#' Create the settings for a fusion ensemble
#' @param type            The type of fusion ensemble pick from: 'uniform', 'AUROC', 'AUPRC' or any
#'                        other metric from evaluationSummary
#' @param evaluation      The evaluation type used to learn the weights (if evaluation is CV and type
#'                        is 'AUROC' then the cross validation AUROC is used to determine the weight
#'                        given to of the base models)
#' @param scaleFunction   How to scale the weights (normalize means weights add up to 1)
#'
#' @examples
#' \dontrun{
#'
#' comSettingNormAUC <- createFusionCombiner(type = "AUROC",
#'                                           evaluation = "CV",
#'                                           scaleFunction = "normalize")
#'
#' comSettingUniform <- createFusionCombiner(type = "uniform",
#'                                           evaluation = "CV",
#'                                           scaleFunction = "normalize")
#'
#' }
#' @export
createFusionCombiner <- function(type = c("uniform", "AUROC", "AUPRC")[1],
                                 evaluation = "CV",
                                 scaleFunction = "normalize") {

  settings <- list(type = type, evaluation = evaluation, scaleFunction = scaleFunction)

  attr(settings, "combineFunction") <- "learnFusion"
  class(settings) <- "combinerSettings"
  return(settings)
}

createPredictionMatrix <- function(predictionList) {

  columnsOfInt <- c("outcomeCount", "ageYear", "gender")

  columnsAvailable <- columnsOfInt[columnsOfInt %in% colnames(predictionList[[1]])]

  prediction <- predictionList[[1]][, c("rowId", "evaluationType", columnsAvailable, "value")]

  predictionList <- lapply(predictionList, function(x) {
    x %>% dplyr::select(.data$rowId, .data$evaluationType, .data$value)
  })

  if (length(predictionList) > 1) {
    for (i in 2:length(predictionList)) {
      prediction <- merge(prediction, predictionList[[i]], by = c("rowId",
                                                                  "evaluationType"), all = T)
    }
  }

  # set and NA values to 0
  prediction[is.na(prediction)] <- 0

  modelNames <- paste0("basemodel_", 1:length(predictionList))

  colnames(prediction) <- c("rowId", "evaluationType", columnsAvailable, modelNames)

  return(prediction)
}

learnFusion <- function(settings, baseModelResults) {

  # get the predictions
  predictions <- lapply(baseModelResults, function(x) {
    x$prediction %>% dplyr::filter(.data$evaluationType == settings$evaluation)
  })

  predictionDF <- createPredictionMatrix(predictions)

  if (settings$type == "uniform") {
    ParallelLogger::logInfo("Uniform weighting")
    weights <- rep(1, length(predictions))
  } else {

    ParallelLogger::logInfo(paste0("Calculating weighting using ",
                                   settings$evaluation,
                                   " and ",
                                   settings$type))

    # get the mertics
    weights <- unlist(lapply(baseModelResults, function(x) {
      x$performanceEvaluation$evaluationStatistics %>% dplyr::filter(.data$evaluation == settings$evaluation &
        .data$metric == settings$type) %>% dplyr::select(.data$value)
    }))
  }

  scaledWeights <- do.call(eval(parse(text = settings$scaleFunction)), list(weights = weights))

  return(list(ensembleFunction = "applyWeightedEnsemble", settings = list(weights = scaledWeights)))

}


applyWeightedEnsemble <- function(settings, predictionList) {
  # create prediction with rowId, outcomeCount, evaluationType, ...
  prediction <- createPredictionMatrix(predictionList)

  modelNames <- grep("basemodel", colnames(prediction))
  prediction$ensemble <- as.matrix(prediction[, modelNames]) %*% as.matrix(settings$weights)

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


# SCALE FUNCTIONS
minmax <- function(weights) {
  ParallelLogger::logInfo("using minmax scaling")
  minVal <- min(weights)
  diffVal <- max(weights) - min(weights)
  newWeights <- (weights - min(weights))/diffVal
  return(newWeights)
}

minNormalize <- function(weights) {
  ParallelLogger::logInfo("using minNormalize scaling")
  minVal <- min(weights)
  sumVal <- sum((weights - min(weights)))
  newWeights <- (weights - min(weights))/sumVal
  return(newWeights)
}

normalize <- function(weights) {
  ParallelLogger::logInfo("using normalize scaling")
  newWeights <- weights/sum(weights)
  return(newWeights)
}


