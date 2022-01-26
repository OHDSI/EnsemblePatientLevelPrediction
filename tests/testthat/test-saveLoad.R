library("testthat")
context("SaveLoad")

ensembleLocation <- file.path(saveDirectory, 'testSave')

test_that("saveEnsemble works", {
  
  saveEnsemble(ensemble, dirPath = ensembleLocation)
  
  expect_true(dir.exists(ensembleLocation))
  
})

test_that("loadEnsemble works", {
  
  ensemble2 <- loadEnsemble(dirPath = ensembleLocation)
  
  expect_true(sum(names(ensemble2) %in% names(ensemble)) == length(names(ensemble)))
  expect_true(class(ensemble) == class(ensemble2))
})

  