library(EnsemblePatientLevelPrediction)
library(FeatureExtraction)
library(PatientLevelPrediction)
library(Eunomia)
# This demo will generate a fusion ensemble consisting of two Logistic Regression models and gradient
# boosting machine.  Dependent on your system it can take some time to run

# We first simulate some data using Eunomia
cat("Press a key to continue")
invisible(readline())

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

covSet <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T,
  useDemographicsAge = T,
  useDemographicsRace = T,
  useDemographicsEthnicity = T,
  useDemographicsAgeGroup = T,
  useConditionGroupEraLongTerm = T,
  useDrugEraStartLongTerm = T,
  endDays = -1
)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main", cdmDatabaseId = 'eunomia',
  cohortDatabaseSchema = "main",
  cohortTable = "cohort",
  targetId = 4,
  outcomeIds = 3,
  outcomeDatabaseSchema = "main",
  outcomeTable = "cohort",
  cdmDatabaseName = "eunomia"
)

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = T,
  washoutPeriod = 365
)

splitSettings <- PatientLevelPrediction::createDefaultSplitSetting()

populationSet <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = F,
  riskWindowStart = 1,
  riskWindowEnd = 730
)


modelDesign1 <- PatientLevelPrediction::createModelDesign(
  targetId = 4,
  outcomeId = 3,
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet,
  runCovariateSummary = F,
  modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
  populationSettings = populationSet,
  preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), 
  splitSettings = splitSettings
)

modelDesign2 <- PatientLevelPrediction::createModelDesign(
  targetId = 4,
  outcomeId = 3,
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet,
  runCovariateSummary = F,
  modelSettings = PatientLevelPrediction::setGradientBoostingMachine(),
  populationSettings = populationSet,
  preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
  splitSettings = splitSettings
)

modelDesign3 <- PatientLevelPrediction::createModelDesign(
  targetId = 4,
  outcomeId = 3,
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet,
  runCovariateSummary = F,
  modelSettings = PatientLevelPrediction::setLassoLogisticRegression(forceIntercept = T),
  populationSettings = populationSet,
  preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
  splitSettings = splitSettings
)


ensembleSettings <- EnsemblePatientLevelPrediction::setEnsembleFromDesign(
  modelDesignList = list(modelDesign1,
                         modelDesign2,
                         modelDesign3),
  databaseDetails = databaseDetails,
  filterSettings = list(minValue = 0.5,
                        maxValue = 1,
                        evaluation = "CV",
                        metric = "AUROC"),
  combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(type = "AUROC",
                                                                          evaluation = "CV",
                                                                          scaleFunction = "normalize")
)

# Now we build the fusion ensemble
cat("Press a key to continue")
invisible(readline())
ensembleResult <- EnsemblePatientLevelPrediction::runEnsemble(
  ensembleSettings = ensembleSettings,
  logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
  saveDirectory = "./testingEnsemble"
)

# You could now save the model and apply it on other data as described in more detail in the
# vignette.
