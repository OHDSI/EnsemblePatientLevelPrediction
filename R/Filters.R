# modify this to be for non saved as well
filterEnsembleFiles <- function(
  filterSettings,
  modelsLocation
){
  
  # load each model in the location
  models <- dir(modelsLocation, pattern = 'Analysis_')
  
  includeModels <- c()
  for(model in models){
    resultLocation <- file.path(modelsLocation, model, 'plpResult')
    plpResult <- PatientLevelPrediction::loadPlpResult(resultLocation)
    
    value <- plpResult$performanceEvaluation$evaluationStatistics %>%
      dplyr::filter(
        .data$evaluation == filterSettings$evaluation & 
          .data$metric == filterSettings$metric
        ) %>%
      dplyr::select(.data$value)
    
    includeMin <- T
    if(!is.null(filterSettings$minValue)){
      if(value < filterSettings$minValue){
        includeMin <- F
      }
    }
    includeMax <- T
    if(!is.null(filterSettings$maxValue)){
      if(value > filterSettings$maxValue){
        includeMax <- F
      }
    }
    
    if(includeMin && includeMax){
      includeModels <- c(includeModels, model)
    }
    
  }
  
  return(includeModels)
}