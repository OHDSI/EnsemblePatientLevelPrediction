# modify this to be for non saved as well
# this results a subset of resultList where only models
# passing the filtering settings are kept
filterBaseModels <- function(resultList, filterSettings) {

  includeModels <- c()
  for (plpResult in resultList) {
    value <- plpResult$performanceEvaluation$evaluationStatistics %>% dplyr::filter(.data$evaluation ==
      filterSettings$evaluation & .data$metric == filterSettings$metric) %>% dplyr::select(.data$value)

    includeMin <- T
    if (!is.null(filterSettings$minValue)) {
      if (value < filterSettings$minValue) {
        includeMin <- F
      }
    }
    includeMax <- T
    if (!is.null(filterSettings$maxValue)) {
      if (value > filterSettings$maxValue) {
        includeMax <- F
      }
    }

    if (includeMin && includeMax) {
      includeModels <- c(includeModels, T)
    } else {
      includeModels <- c(includeModels, F)
    }

  }

  return(resultList[includeModels])
}
