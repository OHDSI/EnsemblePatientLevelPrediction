createEnsemble <- function(combinerSettings, baseModelResults) {

  fun <- eval(parse(text = attr(combinerSettings, "combineFunction")))

  # this creates a plpModel structure
  ensembleModel <- do.call(fun,
                           list(settings = combinerSettings, baseModelResults = baseModelResults))

  result <- list(model = list(ensemble = ensembleModel,
                              baseModels = lapply(baseModelResults, function(x) {
    x$model
  })), trainDetails = list(analysisId = "ensemble"), settings = combinerSettings, covariateImportance = data.frame()  # make this the weights?
)
  class(result) <- "ensembleModel"

  return(result)
}
