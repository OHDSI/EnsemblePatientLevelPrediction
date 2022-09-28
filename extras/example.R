# testing code (requires sequential branch of FeatureExtraction):
rm(list = ls())
library(FeatureExtraction)
# devtools::install_github(repo = 'ohdsi/PatientLevelPrediction', ref = 'issue242')
library(PatientLevelPrediction)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

covSet <- createCovariateSettings(useDemographicsGender = T,
                                  useDemographicsAge = T,
                                  useDemographicsRace = T,
                                  useDemographicsEthnicity = T,
                                  useDemographicsAgeGroup = T,
                                  useConditionGroupEraLongTerm = T,
                                  useDrugEraStartLongTerm = T,
                                  endDays = -1)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = "main",
                                                                 cohortDatabaseSchema = "main",
                                                                 cohortTable = "cohort",
                                                                 targetId = 4,
                                                                 outcomeIds = 3,
                                                                 outcomeDatabaseSchema = "main",
                                                                 outcomeTable = "cohort",
                                                                 cdmDatabaseName = "eunomia")

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(firstExposureOnly = T,
                                                                                 washoutPeriod = 365)

populationSet <- PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = F,
                                                                       riskWindowStart = 1,
                                                                       riskWindowEnd = 730)


modelDesign1 <- PatientLevelPrediction::createModelDesign(targetId = 4,
                                                          outcomeId = 3,
                                                          restrictPlpDataSettings = restrictPlpDataSettings,
                                                          covariateSettings = covSet,
                                                          runCovariateSummary = F,
                                                          modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
                                                          populationSettings = populationSet,
                                                          preprocessSettings = PatientLevelPrediction::createPreprocessSettings())

modelDesign2 <- PatientLevelPrediction::createModelDesign(targetId = 4,
                                                          outcomeId = 3,
                                                          restrictPlpDataSettings = restrictPlpDataSettings,
                                                          covariateSettings = covSet,
                                                          runCovariateSummary = F,
                                                          modelSettings = PatientLevelPrediction::setGradientBoostingMachine(),
                                                          populationSettings = populationSet,
                                                          preprocessSettings = PatientLevelPrediction::createPreprocessSettings())

modelDesign3 <- PatientLevelPrediction::createModelDesign(targetId = 4,
                                                          outcomeId = 3,
                                                          restrictPlpDataSettings = restrictPlpDataSettings,
                                                          covariateSettings = covSet,
                                                          runCovariateSummary = F,
                                                          modelSettings = PatientLevelPrediction::setLassoLogisticRegression(forceIntercept = T),
                                                          populationSettings = populationSet,
                                                          preprocessSettings = PatientLevelPrediction::createPreprocessSettings())

if (F) {
  ensembleSettings <- EnsemblePatientLevelPrediction::setEnsembleFromDesign(modelDesignList = list(modelDesign1,
                                                                                                   modelDesign2,
                                                                                                   modelDesign3),
                                                                            databaseDetails = databaseDetails,
                                                                            filterSettings = list(minValue = 0.5,
                                                                                                  maxValue = 1,
                                                                                                  evaluation = "CV",
                                                                                                  metric = "AUROC"),
                                                                            combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(type = "uniform",
                                                                                                                                                    evaluation = "CV",
                                                                                                                                                    scaleFunction = "normalize"))

  ensemble <- EnsemblePatientLevelPrediction::runEnsemble(ensembleSettings = ensembleSettings,
                                                          logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
                                                          saveDirectory = "./testingEnsemble2")
}

ensembleSettings2 <- EnsemblePatientLevelPrediction::setEnsembleFromDesign(modelDesignList = list(modelDesign1,
                                                                                                  modelDesign2,
                                                                                                  modelDesign3),
                                                                           databaseDetails = databaseDetails,
                                                                           filterSettings = list(minValue = 0.5,
                                                                                                 maxValue = 1,
                                                                                                 evaluation = "CV",
                                                                                                 metric = "AUROC"),
                                                                           combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(type = "AUROC",
                                                                                                                                                   evaluation = "CV",
                                                                                                                                                   scaleFunction = "normalize"))

### debug(EnsemblePatientLevelPrediction:::learnFusion)
ensemble2 <- EnsemblePatientLevelPrediction::runEnsemble(ensembleSettings = ensembleSettings2,
                                                         logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
                                                         saveDirectory = "./testingEnsemble3")
