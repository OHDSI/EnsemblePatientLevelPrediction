library("testthat")
context("Combiners")

test_that("createFusionCombiner works", {
  
  result <- createFusionCombiner(
    type = 'uniform', # 'AUROC', 'AUPRC'
    evaluation = 'CV',
    scaleFunction = 'normalize'
  )
  
  expect_equal(attr(result, 'combineFunction'), 'learnFusion')
  expect_equal(class(result), 'combinerSettings')
  
  expect_true(3 == sum(names(result) %in% c('type', 'evaluation', 'scaleFunction')))
  
}
)

test_that("createFusionCombiner works", {
  
  prediction1 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = runif(5)
    )
  
  prediction2 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = runif(5)
  )
  
  predictionList <- list(prediction1, prediction2)
    
  result <- createPredictionMatrix(predictionList)
  
  expect_true(class(result) == "data.frame")
  expect_true(nrow(result) == 5)
  expect_true(ncol(result) == 7)
  
}
)


test_that("learnFusion works", {
  
  prediction1 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = runif(5)
  )
  
  prediction2 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = runif(5)
  )
  
  res1 <- list(
    prediction = prediction1,
    performanceEvaluation = list(
      evaluationStatistics = data.frame(
        evaluation = 'CV',
        metric = 'AUROC',
        value = 0.8
        )
      )
  )
  
  res2 <- list(
    prediction = prediction2,
    performanceEvaluation = list(
      evaluationStatistics = data.frame(
        evaluation = 'CV',
        metric = 'AUROC',
        value = 0.7
      )
    )
  )
  
result <- learnFusion(
  settings = list(evaluation = 'CV',
                  type = 'AUROC',
                  scaleFunction  = 'normalize'),
  baseModelResults = list(res1, res2)
)

expect_equal(result$ensembleFunction, 'applyWeightedEnsemble')
expect_true(!is.null(result$settings$weights))
  
  }
)


test_that("applyWeightedEnsemble works", {
  
  values1 <- runif(5)
  values2 <- runif(5)
  prediction1 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = values1
  )
  attr(prediction1, 'metaData') <- list(a=1)
  
  prediction2 <- data.frame(
    rowId = 1:5, 
    evaluationType = rep('CV',5),
    outcomeCount = rep(0,5),
    ageYear = c(13,54,23,64,23),
    gender = rep('female', 5),
    value = values2
  )
  
  predictionList <- list(prediction1, prediction2)
  
  settings <-list(weights = c(1,1))
  
  result <- applyWeightedEnsemble(settings, predictionList)
  
  enResult <- result %>% dplyr::filter(evaluationType == 'CV_ensemble')
  
  # sum be sum of preds as weights are 1,1
  expect_true(sum(as.double(enResult$value) == (values1+values2)) == length(values1))
  
  # should have prediction 1 metaData
  expect_equal(attr(result, 'metaData'), list(a=1))
  
})


test_that("minmax works", {
  
  weights <- c(0.8,0.5,0.9)
  result <- minmax(weights)
  
  expect_equal(result, c(0.3,0,0.4)/0.4)
})


test_that("minNormalize works", {
  
  weights <- c(0.8,0.5,0.9)
  result <- minNormalize(weights)
  
  expect_equal(result, c(0.3,0,0.4)/0.7)
})

test_that("normalize works", {
  
  weights <- c(0.8,0.5,0.9)
  result <- normalize(weights)
  
  expect_equal(result, c(0.8,0.5,0.9)/(0.8+0.5+0.9))
})
