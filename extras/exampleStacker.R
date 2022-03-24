library(EnsemblePatientLevelPrediction)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main", 
  cohortTable = "cohort", 
  cohortId = 4, 
  outcomeIds = 3,
  outcomeDatabaseSchema = "main", 
  outcomeTable =  "cohort", 
  cdmDatabaseName = 'eunomia'
)

targetId <- 4

# 2. specify the cohort_definition_id for the outcome
outcomeId <- 3 

# 3. specify the covariates
# we include gender, age, race, ethnicity, age in 5 year groups,
# conditions groups (using the vocabulary hierarchy) in the past
# 365 days and drug ingredients in the past 365 days
# we do not include conditions/drugs that occur at index
covSet <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAge = T, 
  useDemographicsRace = T,
  useDemographicsEthnicity = T, 
  useDemographicsAgeGroup = T,
  useConditionGroupEraLongTerm = T, 
  useDrugEraStartLongTerm  = T, 
  endDays = -1
)

# 4. specify inclusion for data extraction
# we restrict to patients in the target cohort with >=365 days
# observation prior to index and only include patients once 
# (the first cohort entry)
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = T, 
  washoutPeriod = 365
)

# 5. specify inclusion and time-at-risk
# We are predicting the outcome occurring from 1 day after index until
# 730 days after index.  We keep patients who drop out during the 
# time-at-risk.
populationSet <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = F, 
  riskWindowStart = 1, 
  riskWindowEnd = 730
)

# 6. specify the model
# We train a default LASSO logistic regression model
modelSetting <- PatientLevelPrediction::setLassoLogisticRegression()

# 7. specify the preprocessing
# We use default preprocessing (normalize data and remove rare/redundant covariates)
preprocessSettings <- PatientLevelPrediction::createPreprocessSettings()

# input these into PatientLevelPrediction::createModelDesign
# this generates a modelDesign object
modelDesign1 <- PatientLevelPrediction::createModelDesign(
  targetId = targetId,
  outcomeId = outcomeId, 
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet, 
  runCovariateSummary = F,
  modelSettings = modelSetting,
  populationSettings = populationSet, 
  preprocessSettings = preprocessSettings
)

targetId <- 4

# 2. specify the cohort_definition_id for the outcome
outcomeId <- 3 

# 3. specify the covariates
# we include gender, age, race, ethnicity, age in 5 year groups,
# conditions groups (using the vocabulary hierarchy) in the past
# 365 days and drug ingredients in the past 365 days
# we do not include conditions/drugs that occur at index
covSet <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAge = T, 
  useDemographicsRace = T,
  useDemographicsEthnicity = T, 
  useDemographicsAgeGroup = T,
  useConditionGroupEraLongTerm = T, 
  useDrugEraStartLongTerm  = T, 
  endDays = -1
)

# 4. specify inclusion for data extraction
# we restrict to patients in the target cohort with >=365 days
# observation prior to index and only include patients once 
# (the first cohort entry)
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = T, 
  washoutPeriod = 365
)

# 5. specify inclusion and time-at-risk
# We are predicting the outcome occurring from 1 day after index until
# 730 days after index.  We keep patients who drop out during the 
# time-at-risk.
populationSet <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = F, 
  riskWindowStart = 1, 
  riskWindowEnd = 730
)

# 6. specify the model
# We train a gradient boosting machine with default 
# hyper-parameter search
modelSetting <- PatientLevelPrediction::setGradientBoostingMachine()

# 7. specify the preprocessing
# We use default preprocessing (normalize data and remove rare/redundant covariates)
preprocessSettings <- PatientLevelPrediction::createPreprocessSettings()

modelDesign2 <- PatientLevelPrediction::createModelDesign(
  targetId = targetId,
  outcomeId = outcomeId, 
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet, 
  runCovariateSummary = F,
  modelSettings = modelSetting,
  populationSettings = populationSet, 
  preprocessSettings = preprocessSettings
)

targetId <- 4

# 2. specify the cohort_definition_id for the outcome
outcomeId <- 3 

# 3. specify the covariates
# we include gender, age, race, ethnicity, age in 5 year groups,
# conditions groups (using the vocabulary hierarchy) in the past
# 365 days and drug ingredients in the past 365 days
# we do not include conditions/drugs that occur at index
# we also include measurements indicators in the past 365 days
covSet <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAge = T, 
  useDemographicsRace = T,
  useDemographicsEthnicity = T, 
  useDemographicsAgeGroup = T,
  useConditionGroupEraLongTerm = T, 
  useDrugEraStartLongTerm  = T, 
  useMeasurementLongTerm = T,
  endDays = -1
)

# 4. specify inclusion for data extraction
# we restrict to patients in the target cohort with >=365 days
# observation prior to index and only include patients once 
# (the first cohort entry)
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = T, 
  washoutPeriod = 365
)

# 5. specify inclusion and time-at-risk
# We are predicting the outcome occurring from 1 day after index until
# 730 days after index.  We keep patients who drop out during the 
# time-at-risk.
populationSet <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = F, 
  riskWindowStart = 1, 
  riskWindowEnd = 730
)

# 6. specify the model
# We train a LASSO logistic regression model but include the 
# intercept in the regularization
modelSetting <- PatientLevelPrediction::setLassoLogisticRegression(
  forceIntercept = T
)

# 7. specify the preprocessing
# We use default preprocessing (normalize data and remove rare/redundant covariates)
preprocessSettings <- PatientLevelPrediction::createPreprocessSettings()

modelDesign3 <- PatientLevelPrediction::createModelDesign(
  targetId = targetId,
  outcomeId = outcomeId, 
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet, 
  runCovariateSummary = F,
  modelSettings = modelSetting,
  populationSettings = populationSet, 
  preprocessSettings = preprocessSettings
)

splitSettings <- PatientLevelPrediction::createDefaultSplitSetting(
  testFraction = 0.5, 
  trainFraction = 0.5
)

filterSettings <- list(
  metric = 'AUROC',
  evaluation = 'CV',
  minValue = 0.5, 
  maxValue = 1
)

combinerSettings <- EnsemblePatientLevelPrediction::createStackerCombiner(
  levelTwoType = 'logisticRegressionStacker',
  levelTwoDataSettings = list(type = 'Test', proportion = 0.5)
)

ensembleSettings <- setEnsembleFromDesign(
  modelDesignList = list(modelDesign1, modelDesign2, modelDesign3),
  databaseDetails = databaseDetails,
  splitSettings = splitSettings,
  filterSettings = filterSettings,
  combinerSettings = combinerSettings
)

ensemble <- runEnsemble(
  ensembleSettings = ensembleSettings,
  logSettings = PatientLevelPrediction::createLogSettings(logName = 'ensemble'),
  saveDirectory = './testingEnsemble'
)

# repeat but use CV instead of test for stacker training

combinerSettings2 <- EnsemblePatientLevelPrediction::createStackerCombiner(
  levelTwoType = 'logisticRegressionStacker',
  levelTwoDataSettings = list(type = 'CV')
)

ensembleSettings2 <- setEnsembleFromDesign(
  modelDesignList = list(modelDesign1, modelDesign2, modelDesign3),
  databaseDetails = databaseDetails,
  splitSettings = splitSettings,
  filterSettings = filterSettings,
  combinerSettings = combinerSettings2
)

ensemble2 <- runEnsemble(
  ensembleSettings = ensembleSettings2,
  logSettings = PatientLevelPrediction::createLogSettings(logName = 'ensemble'),
  saveDirectory = './testingEnsemble2'
)
