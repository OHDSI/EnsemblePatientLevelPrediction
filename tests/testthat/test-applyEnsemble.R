library("testthat")
context("ApplyEnsemble")


#test_that("applyEnsemble inputs", {
  
  #applyEnsemble(
  #  ensemble,
  #  newDatabaseDetails,
  #  logSettings = PatientLevelPrediction::createLogSettings(),
  #  outputFolder
  #)
  
#})

test_that("applyEnsembleToPredictions works", {
  
  ensembleTest <- list(
    model = list(
      ensemble = list(
        ensembleFunction = 'applyWeightedEnsemble',
        settings = list(weights = c(0.2,0.3)
      )
                               )
    )
  )
  
  prediction1 <- data.frame(
    rowId = 1, 
    evaluationType = 'CV', 
    ageYear = 40,
    gender = 'male',
    outcomeCount = 0,
    value = 0.5
    )
  prediction2 <- data.frame(
    rowId = 1, 
    evaluationType = 'CV', 
    ageYear = 40,
    gender = 'male',
    outcomeCount = 0,
    value = 0.3
  )
  
  predictionList <- list(prediction1, prediction2)

result <- applyEnsembleToPredictions(
  predictionList = predictionList,
  ensemble = ensembleTest
)

valueExpected <- 0.5*0.2 +0.3*0.3

# ensemble is correct
expect_equal(as.double(result %>% 
  dplyr::filter(.data$evaluationType == 'CV_ensemble') %>%
  dplyr::select(.data$value)), valueExpected)

# model_1 is correct
expect_equal(as.double(result %>% 
                         dplyr::filter(.data$evaluationType == 'CV_basemodel_1') %>%
                         dplyr::select(.data$value)), 0.5)

# model_2 is correct
expect_equal(as.double(result %>% 
                         dplyr::filter(.data$evaluationType == 'CV_basemodel_2') %>%
                         dplyr::select(.data$value)), 0.3)
  
})