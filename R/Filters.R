#' Filter our base level 1 models that do not perform sufficiently well
#' @param resultList      A list of runPlp() result objects (the level 1 models that are candidates for the ensemble level 1 models)
#' @param filterSettings  A list containing the settings that specify how to filter the candidate level 1 models
#'
#' @examples
#' 
#' \dontrun{
#'
#' # only include models with an AUROC of 0.7 or higher for cross validation
#'
#' level1Models <- filterBaseModels(
#' resultList, 
#' filterSettings = list(
#' evaluation = 'CV',
#' metric = 'AUROC',
#' minValue = 0.7
#' )
#' )
#'
#' }
#' @export
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
