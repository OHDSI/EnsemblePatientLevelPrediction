library("testthat")
context("runEnsemble")

test_that("runEnsemble works", {
  
  expect_true(dir.exists(file.path(saveDirectory, 'Ensemble')))
  
  expect_true(inherits(ensemble, 'plpEnsemble'))

})
