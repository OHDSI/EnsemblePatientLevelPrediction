saveEnsemble <- function(ensemble, dirPath){
  
  if(!dir.exists(file.path(dirPath, 'ensemble'))){
    dir.create(file.path(dirPath, 'ensemble'), recursive = T)
  }
  
  # save the ensemble
  saveEnsembleModel(
    ensembleModel = ensemble$model, 
    dirPath = file.path(dirPath, 'ensemble')
  )
  
  ensemble$model <- NULL
  saveRDS(ensemble, file = file.path(dirPath, "ensemblePlp.rds"))
  
}

loadEnsemble <- function(dirPath){
  
  ensemble <- readRDS(file = file.path(dirPath, "ensemblePlp.rds"))
  ensemble$model <- loadEnsembleModel(dirPath = file.path(dirPath, 'ensemble'))
  
  return(ensemble)
}


saveEnsembleModel <- function(ensembleModel, dirPath){
  
  if(!dir.exists(file.path(dirPath, 'base'))){
    dir.create(file.path(dirPath, 'base'), recursive = T)
  }
  
  # save the models - not needed if models already saved
  if(class(ensembleModel$model$baseModels[[1]]) == 'plpModel'){
    for(i in 1:length(ensembleModel$model$baseModels)){
      PatientLevelPrediction::savePlpModel(
        plpModel = ensembleModel$model$baseModels[[i]], 
        dirPath = file.path(dirPath, 'base', paste0('basemodel_',i) )
      )
    }
    
    #save the ensemble
    baseModels <- lapply(
      1:length(ensembleModel$model$baseModels), 
      function(i){
        paste0('base/', paste0('basemodel_',i))
      } 
    )
    
    ensembleModel$model$baseModels <- baseModels
  }
  
  class(ensembleModel) <- 'plpModel'
  attr(ensembleModel, "saveType") <- "RtoJson"
  
  PatientLevelPrediction::savePlpModel(
    plpModel = ensembleModel, 
    dirPath = file.path(dirPath)
  )
  
}

loadEnsembleModel <- function(dirPath){
  
  # load the ensemble 
  ensembleModel <- PatientLevelPrediction::loadPlpModel(
    dirPath = file.path(dirPath)
  )
  
  # load the base models based on there locations
  ensembleModel$model$baseModels <- lapply(
    ensembleModel$model$baseModels, 
    function(x){
      PatientLevelPrediction::loadPlpModel(dirPath = file.path(dirPath, x))
    }
  )
  
  class(ensembleModel) <- 'ensembleModel'
  return(ensembleModel) 
}