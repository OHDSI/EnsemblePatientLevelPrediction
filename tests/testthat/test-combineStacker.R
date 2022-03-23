library("testthat")
context("Stacker Combiners")

test_that("createStackerCombiner works", {
  
  result <- createStackerCombiner(
    levelTwoType = "logisticRegressionStacker",
    levelTwoHyperparameters = NULL,
    levelTwoDataSettings = list(type = 'Test', proportion = 0.5)
  )
  
  expect_equal(attr(result, 'combineFunction'), 'learnStacker')
  expect_equal(class(result), 'combinerSettings')
  
  expect_true(!is.null(result$levelTwoDataSettings$seed))
  
}
)

test_that("learnStacker works", {
  
  # random prediction
  prediction1 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)), 
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)),
    outcomeCount = sample(2,1000, replace = T)-1,
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = runif(1000)
  )
  
  # resonable prediction
  prediction2 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)), 
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)
    ),
    outcomeCount = c(rep(0,800), rep(1, 200)),
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = c(runif(800)/2, runif(200))
  )
  
  # better prediction
  prediction3 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)),  
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)
    ),
    outcomeCount = c(rep(0,800), rep(1, 200)),
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = c(runif(800)/10, runif(200))
  )
  
  res1 <- list(
    prediction = prediction1,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  res2 <- list(
    prediction = prediction2,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  res3 <- list(
    prediction = prediction3,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  enSet <- createStackerCombiner(
    levelTwoType = "logisticRegressionStacker",
    levelTwoHyperparameters = NULL,
    levelTwoDataSettings = list(type = 'Test', proportion = 0.5)
  )
  
  result <- learnStacker(
    settings = enSet,
    baseModelResults = list(res1, res2, res3)
  )
  
  # check result
  expect_true(result$settings$stackerModel$family$family == 'binomial')
  expect_true(length(result$settings$stackerModel$coefficients) == 4)
  
  # check prediction
  pred <-  applyStackerEnsemble(
    settings = result$settings, 
    predictionList = lapply(
      list(res1, res2, res3),
      function(x){x$prediction}
      )
    )
  
  expect_true(nrow(pred) == nrow(res1$prediction)*4)

  # no negative predictions
  expect_true(sum(pred$value < 0) == 0)
  
  # no predictions > 1
  expect_true(sum(pred$value > 1) == 0)
  
  expect_true(sum(table(pred$evaluationType) == 500) == 8)
  
}
)

test_that("partionTestData works", {
  
  # random prediction
  prediction1 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)), 
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)),
    outcomeCount = sample(2,1000, replace = T)-1,
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = runif(1000)
  )
  
  # resonable prediction
  prediction2 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)), 
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)
    ),
    outcomeCount = c(rep(0,800), rep(1, 200)),
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = c(runif(800)/2, runif(200))
  )
  
  # better prediction
  prediction3 <- data.frame(
    rowId = c(rep(1:400,2),rep(401:500,2)),  
    evaluationType = c(rep('CV',400), rep('Test',400),
                       rep('CV',100), rep('Test',100)
    ),
    outcomeCount = c(rep(0,800), rep(1, 200)),
    ageYear = rep(c(13,54,23,64,23),200),
    gender = rep('female', 1000),
    value = c(runif(800)/10, runif(200))
  )
  
  res1 <- list(
    prediction = prediction1,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  res2 <- list(
    prediction = prediction2,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  res3 <- list(
    prediction = prediction3,
    performanceEvaluation = list(
      evaluationStatistics = data.frame()
    )
  )
  
  enSet <- createStackerCombiner(
    levelTwoType = "logisticRegressionStacker",
    levelTwoHyperparameters = NULL,
    levelTwoDataSettings = list(type = 'Test', proportion = 0.5)
  )
  
  resList2 <- partionTestData(
    resultList = list(res1, res2, res3),
    ensembleSettings = enSet
  )
  
  expect_equal(
    sum(
      resList2[[1]]$prediction$evaluationType == 'levelTwoTest' |
        resList2[[1]]$prediction$evaluationType == 'Test'
    ), 
    500
  )
  
  expect_equal(
    sum(resList2[[1]]$prediction$evaluationType == 'levelTwoTest'),
    250
  )
  expect_equal(
    sum(resList2[[2]]$prediction$evaluationType == 'levelTwoTest'),
    250
  )  
  expect_equal(
    sum(resList2[[3]]$prediction$evaluationType == 'levelTwoTest'),
    250
  )
  
}
)