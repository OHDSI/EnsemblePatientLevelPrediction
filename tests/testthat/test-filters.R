library("testthat")
context("Filters")

test_that("filterBaseModels works", {
  
  plpResult1 <- list(
    name = 'include',
    performanceEvaluation = list(
      evaluationStatistics = data.frame(
        evaluation = 'Test',
        metric = 'madeUp',
        value = 0.8
      )
    )
  )
  
  plpResult2 <- list(
    name = 'NotInclude',
    performanceEvaluation = list(
      evaluationStatistics = data.frame(
        evaluation = 'Test',
        metric = 'madeUp',
        value = 0.6
      )
    )
  )
  
  plpResult3 <- list(
    name = 'include',
    performanceEvaluation = list(
      evaluationStatistics = data.frame(
        evaluation = 'Test',
        metric = 'madeUp',
        value = 0.7
      )
    )
  )
  
  resultList <- list(plpResult1, plpResult2, plpResult3)

result <- filterBaseModels(
  resultList = resultList,
  filterSettings = list(
    evaluation = 'Test',
    metric = 'madeUp',
    minValue = 0.7,
    maxValue = 0.9
    )
)

# should remove middle one
expect_true(length(result) == 2)

expect_equal(unique(unlist(lapply(result, function(x) x$name))), c('include'))
  
})
