library("testthat")
context("SetEnsemble")

test_that("setEnsembleFromDesign works", {

fromDesign <- setEnsembleFromDesign(
  modelDesignList = list(modelDesign1, modelDesign2),
  databaseDetails = databaseDetails,
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
  filterSettings = list(
    minValue = 0.5, 
    maxValue = 1,
    evaluation = 'CV',
    metric = 'AUROC'
  ),
  combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(
    type = 'AUROC',
    evaluation = 'CV',
    scaleFunction = 'normalize'
  )
)

expectedNames <- c(
  'executionList', 'databaseDetails', 
  'modelDesignList', 'splitSettings', 
  'filterSettings', 'combinerSettings'
)

expect_true(sum(names(fromDesign) %in% expectedNames) == length(expectedNames))
expect_true(fromDesign$executionList$evaluateEnsemble)

})

test_that("setEnsembleFromResults works", {
  
  runPlp1 <- list(
    prediction = data.frame(
    subjectId = 1:5, cohortStartDate = 1:5
  )
  )
  class(runPlp1) <- 'runPlp'
  
  runPlp2 <- list(
    prediction = data.frame(
      subjectId = 11:15, cohortStartDate = 1:5
    )
  )
  class(runPlp2) <- 'runPlp'
  
  fromResults <- setEnsembleFromResults(
    resultList = list(runPlp1, runPlp2),
    filterSettings = list(
      minValue = 0.5, 
      maxValue = 1,
      evaluation = 'CV',
      metric = 'AUROC'
    ),
    combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(
      type = 'AUROC',
      evaluation = 'CV',
      scaleFunction = 'normalize'
    )
  )
  
  expectedNames <- c(
    'executionList', 
    'resultList',
    'filterSettings', 
    'combinerSettings'
  )
  
  expect_true(sum(names(fromResults) %in% expectedNames) == length(expectedNames))
  expect_true(!fromResults$executionList$evaluateEnsemble)
  
})

test_that("setEnsembleFromFiles works", {
  
  runPlpLoc1 <- file.path(saveDirectory, 'Analysis_1', 'plpResult')
  runPlpLoc2 <- file.path(saveDirectory, 'Analysis_3', 'plpResult')
  
  fromResults <- setEnsembleFromFiles(
    fileVector = c(runPlpLoc1, runPlpLoc2),
    filterSettings = list(
      minValue = 0.5, 
      maxValue = 1,
      evaluation = 'CV',
      metric = 'AUROC'
    ),
    combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(
      type = 'AUROC',
      evaluation = 'CV',
      scaleFunction = 'normalize'
    )
  )
  
  expectedNames <- c(
    'executionList', 
    'resultList',
    'filterSettings', 
    'combinerSettings'
  )
  
  expect_true(sum(names(fromResults) %in% expectedNames) == length(expectedNames))
  expect_true(!fromResults$executionList$evaluateEnsemble)
  
})