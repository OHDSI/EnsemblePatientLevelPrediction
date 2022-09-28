#' Save an ensemble result
#' @param ensemble   An ensemblePlp created by \code{EnsemblePatientLevelPrediction::runEnsemble()}
#' @param dirPath    The directory to save the ensemble result
#'
#' @examples
#' \dontrun{
#'
#' saveEnsemble(ensemble, "C:/bestEnsemble")
#'
#' }
#' @export
saveEnsemble <- function(ensemble, dirPath) {

  if (!dir.exists(file.path(dirPath, "ensemble"))) {
    dir.create(file.path(dirPath, "ensemble"), recursive = T)
  }

  # save the ensemble
  saveEnsembleModel(
    ensembleModel = ensemble$model, 
    dirPath = file.path(dirPath, "ensemble")
    )

  ensemble$model <- NULL
  saveRDS(
    object = ensemble, 
    file = file.path(dirPath, "ensemblePlp.rds")
    )

}

#' Load a previously saved ensemble result
#' @param dirPath   The directory to the saved ensemble result
#'
#' @examples
#' \dontrun{
#'
#' ensemble <- loadEnsemble("C:/bestEnsemble")
#'
#' }
#' @export
loadEnsemble <- function(dirPath) {

  ensemble <- readRDS(file = file.path(dirPath, "ensemblePlp.rds"))
  ensemble$model <- loadEnsembleModel(
    dirPath = file.path(dirPath, "ensemble")
    )

  return(ensemble)
}

#' Save an ensemble model (object of class ensembleModel)
#' @param ensembleModel   An ensembleModel
#' @param dirPath         The directory to save the ensemble model
#'
#' @examples
#' \dontrun{
#'
#' saveEnsembleModel(ensembleModel, "C:/bestEnsembleModel")
#'
#' }
#' @export
saveEnsembleModel <- function(ensembleModel, dirPath) {

  if (!dir.exists(file.path(dirPath, "base"))) {
    dir.create(file.path(dirPath, "base"), recursive = T)
  }

  # save the models - not needed if models already saved
  if (inherits(ensembleModel$model$baseModels[[1]], "plpModel")) {
    for (i in 1:length(ensembleModel$model$baseModels)) {
      PatientLevelPrediction::savePlpModel(
        plpModel = ensembleModel$model$baseModels[[i]],
        dirPath = file.path(
          dirPath,
          "base",
          paste0("basemodel_", i)
          )
        )
    }

    # save the ensemble
    baseModels <- lapply(1:length(ensembleModel$model$baseModels), function(i) {
      paste0("base/", paste0("basemodel_", i))
    })

    ensembleModel$model$baseModels <- baseModels
  }

  class(ensembleModel) <- "plpModel"
  attr(ensembleModel, "saveType") <- "RtoJson"

  PatientLevelPrediction::savePlpModel(plpModel = ensembleModel, dirPath = file.path(dirPath))

}

#' Load a previously saved ensemble model (object of class ensembleModel)
#' @param dirPath   The directory containing the saved ensemble model
#'
#' @examples
#' \dontrun{
#'
#' ensembleModel <- loadEnsembleModel("C:/bestEnsembleModel")
#'
#' }
#' @export
loadEnsembleModel <- function(dirPath) {

  # load the ensemble
  ensembleModel <- PatientLevelPrediction::loadPlpModel(dirPath = file.path(dirPath))

  # load the base models based on there locations
  ensembleModel$model$baseModels <- lapply(ensembleModel$model$baseModels, function(x) {
    PatientLevelPrediction::loadPlpModel(dirPath = file.path(dirPath, x))
  })

  class(ensembleModel) <- "ensembleModel"
  return(ensembleModel)
}
