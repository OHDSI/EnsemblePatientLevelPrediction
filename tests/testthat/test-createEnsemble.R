library("testthat")
context("CreateEnsemble")

test_that("createEnsemble works", {
  
  combinerSettings <- createFusionCombiner(
    type = 'uniform',
    evaluation = 'CV',
    scaleFunction = 'normalize'
  )
  
  baseModelResults <- lapply(
    1:3, 
    function(i){
      PatientLevelPrediction::loadPlpResult(
        file.path(
          saveDirectory, 
          paste0('Analysis_',i),
          'plpResult'
          )
        )
    }
  )
    
  result <- createEnsemble(
    combinerSettings,
    baseModelResults
  )
  
  expect_true(inherits(result, 'ensembleModel'))
    
}
)
  
