saveDirectory <- tempdir()

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
  cdmDatabaseSchema = "main",
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
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting()
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
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting()
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
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting()
)


ensembleSettings <- EnsemblePatientLevelPrediction::setEnsembleFromDesign(
  modelDesignList = list(
    modelDesign1,
    modelDesign2,
    modelDesign3
  ),
  databaseDetails = databaseDetails,
  filterSettings = list(
    minValue = 0.5,
    maxValue = 1,
    evaluation = "CV",
    metric = "AUROC"
  ),
  combinerSettings = EnsemblePatientLevelPrediction::createFusionCombiner(
    type = "uniform",
    evaluation = "CV",
    scaleFunction = "normalize"
  )
)

ensemble <- EnsemblePatientLevelPrediction::runEnsemble(
  ensembleSettings = ensembleSettings,
  logSettings = PatientLevelPrediction::createLogSettings(logName = "ensemble"),
  saveDirectory = saveDirectory
)
